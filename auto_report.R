# 1. 라이브러리 로드 및 테마 설정
library(bit); library(bit64); library(datarizer); library(DBI); library(RMySQL)
library(ggplot2); library(dplyr); library(lubridate); library(data.table)
library(tidyr); library(gridExtra); library(glue); library(grid)

theme_table <- gridExtra::ttheme_default(
  core    = list(fg_params = list(cex = 0.8)),
  colhead = list(fg_params = list(cex = 0.9, fontface = "bold")),
  padding = unit(c(3,2), "mm")
)

# 2. 환경 설정
Sys.setenv(TZ = "Asia/Seoul")
SITE_ID     <- "11610"
today_label <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
BASE_DIR <- glue("/home/namsangkim/item-reports/item-report-{SITE_ID}")
setwd(BASE_DIR)


# 3. 데이터 로드
site_mst <- get_query(
  to = "athena",
  glue("SELECT * FROM dashboard.site_mst WHERE site_id = '{SITE_ID}'"),
  output_type = "data.table"
)
SITE_NAME <- site_mst[site_id == SITE_ID]$site_name

item_mst <- get_query(
  to = "athena",
  glue("SELECT item_id, item_name FROM src_meta.item_mst WHERE site_id = '{SITE_ID}'"),
  output_type = "data.table"
) %>% filter(!grepl("사은품", item_name))

item_profile_dt <- get_query(
  to = "athena",
  glue("SELECT * FROM profiling.item_profile_daily
         WHERE site_id = '{SITE_ID}'
           AND DATE(date_id) BETWEEN DATE_ADD('day', -14, CURRENT_DATE)
                                     AND DATE_ADD('day',  -1, CURRENT_DATE)"),
  output_type = "data.table"
) %>% inner_join(item_mst, by = "item_id")

# 4. 일별 트렌드 생성
daily_item_trend <- item_profile_dt %>%
  mutate(
    date       = ymd(date_id),
    imp        = imp_cnt,
    click      = click_cnt,
    view       = view_cnt,
    sales_cnt  = conversion_cnt,
    CTR        = pmin(round(click/imp*100,2), 100),
    CVR        = pmin(round(sales_cnt/view*100,2), 100),
    CTR        = ifelse(is.nan(CTR), 0, CTR),
    CVR        = ifelse(is.nan(CVR), 0, CVR)
  ) %>%
  group_by(item_id, item_name, date) %>%
  summarise(imp, click, view, sales_cnt, CTR = mean(CTR), CVR = mean(CVR), .groups = "drop") %>%
  group_by(date) %>%
  mutate(total_sales = sum(sales_cnt)) %>%
  ungroup() %>%
  mutate(sales_share = round(sales_cnt/total_sales*100, 2))

# 5. 기간 설정
latest_date <- max(daily_item_trend$date)
recent_week <- seq(latest_date - 6, latest_date, by = "day")
prev_week   <- seq(latest_date - 13, latest_date - 7, by = "day")

# 6. Top100 상품 선정
top_items <- daily_item_trend %>%
  filter(date %in% c(prev_week, recent_week)) %>%
  group_by(item_id, item_name) %>%
  summarise(total_sales = sum(sales_cnt), .groups = "drop") %>%
  arrange(desc(total_sales)) %>%
  slice_head(n = 100)

# 7. 요약 생성 함수
make_summary <- function(data, items, week, suffix) {
  data %>%
    filter(item_id %in% items$item_id, date %in% week, imp > 0) %>%
    group_by(item_id, item_name) %>%
    summarise(
      avg_sales = mean(sales_cnt),
      CTR = mean(CTR, na.rm = TRUE),
      CVR = mean(CVR, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(CTR = ifelse(is.nan(CTR), 0, CTR),
           CVR = ifelse(is.nan(CVR), 0, CVR)) %>%
    rename_with(~ paste0(.x, "_", suffix), c("avg_sales", "CTR", "CVR"))
}

recent_sum <- make_summary(daily_item_trend, top_items, recent_week, "recent")
prev_sum   <- make_summary(daily_item_trend, top_items, prev_week, "prev")

# 8. 비교 및 순위 계산
summary_compare <- inner_join(recent_sum, prev_sum, by = c("item_id", "item_name")) %>%
  arrange(desc(avg_sales_recent), desc(CVR_recent), desc(CTR_recent)) %>%
  mutate(rank_recent = row_number()) %>%
  arrange(desc(avg_sales_prev), desc(CVR_prev), desc(CTR_prev)) %>%
  mutate(rank_prev = row_number()) %>%
  mutate(rank_diff = rank_prev - rank_recent) %>%
  arrange(rank_recent, desc(CTR_recent), desc(CVR_recent))

# 🎯 여백 최소화 테마 정의
theme_table <- ttheme_default(
  core    = list(fg_params = list(cex = 0.8)),
  colhead = list(fg_params = list(cex = 0.9, fontface = "bold")),
  padding = unit(c(1, 1), "mm")  # ← 여백 최소화
)

# 📄 이미지 저장 함수
save_table <- function(df, filename, top_n = 10) {
  tbl <- df %>%
    slice_head(n = top_n) %>%
    mutate(
      번호 = row_number(),
      `상품명 (ID)` = paste0(item_name, " (", item_id, ")"),
      `평균 판매량` = paste0(round(avg_sales_recent,1), " (", round(avg_sales_prev,1), ")"),
      CTR = paste0(round(CTR_recent,2), " (", round(CTR_prev,2), ")"),
      CVR = paste0(round(CVR_recent,2), " (", round(CVR_prev,2), ")"),
      `판매 순위` = paste0(rank_recent, "위 (", rank_prev, "위)")
    ) %>%
    select(번호, `상품명 (ID)`, `평균 판매량`, CTR, CVR, `판매 순위`)
  
  tbl_grob <- tableGrob(tbl, rows = NULL, theme = theme_table)
  
  ggsave(
    filename = filename,
    plot = tbl_grob,
    width = 14,
    height = ifelse(top_n <= 10, 6, 8),
    dpi = 300
  )
}

save_table(summary_compare, "summary_7day_compare_table.png", top_n = 30)
save_table(summary_compare %>% arrange(desc(rank_diff)), "summary_rank_change_top10_table.png", top_n = 10)

# 10. 트렌드 플롯 저장 함수
plot_trends <- function(df_all, summary_df, filename) {
  x_limits <- range(df_all$date)
  x_breaks <- seq(x_limits[1], x_limits[2], length.out = 3)
  
  plots <- lapply(seq_len(nrow(summary_df)), function(i) {
    row <- summary_df[i, ]
    df <- df_all %>%
      filter(item_id == row$item_id) %>%
      filter(!is.na(CTR) & !is.na(CVR) & !is.na(sales_cnt))
    
    scaleFactor <- max(df$sales_cnt, na.rm = TRUE) / 100
    
    ggplot(df, aes(x = date)) +
      geom_line(aes(y = CTR, color = "CTR"), linewidth = 1, na.rm = TRUE) +
      geom_line(aes(y = CVR, color = "CVR"), linewidth = 1, na.rm = TRUE) +
      geom_line(aes(y = sales_cnt / scaleFactor, linetype = "판매수량"), linewidth = 0.8, na.rm = TRUE) +
      scale_x_date(limits = x_limits, breaks = x_breaks, date_labels = "%b %d") +
      scale_y_continuous(
        name = "비율 (%)", limits = c(0, 100),
        sec.axis = sec_axis(~ . * scaleFactor, name = "판매수량")
      ) +
      labs(title = paste0(row$item_name, " (", row$item_id, ")"), x = "날짜") +
      theme_minimal() +
      theme(legend.position = "bottom", axis.title.y.right = element_text(color = "black"))
  })
  
  png(filename, width = 1600, height = 2000, res = 150)
  do.call(grid.arrange, c(plots, ncol = 2))
  dev.off()
}

# 실행
top10_change <- summary_compare %>% arrange(desc(rank_diff)) %>% slice_head(n = 10)
plot_trends(daily_item_trend, top10_change, "rank_change_top10_trend.png")


# 이미지 저장 후 약간 대기 (파일 저장 완료 보장)
Sys.sleep(1)

# ✅ 자바스크립트 코드 따로 작성
js_code <- '
<script src="https://cdn.jsdelivr.net/npm/js-sha256@0.9.0/src/sha256.min.js"></script>
<script>
  const PLAIN_PASSWORD = "dr_11610";

function checkPassword() {
  const input = document.getElementById("pw").value.trim();
  if (input === PLAIN_PASSWORD) {
    document.getElementById("auth-box").style.display = "none";
    document.getElementById("main-content").style.display = "block";
    document.getElementById("defaultOpen")?.click();
  } else {
    alert("비밀번호가 틀렸습니다.");
  }
}

  function openTab(evt, tabName) {
    const tabcontent = document.getElementsByClassName("tabcontent");
    const tablinks = document.getElementsByClassName("tablinks");
    for (let i = 0; i < tabcontent.length; i++) tabcontent[i].style.display = "none";
    for (let i = 0; i < tablinks.length; i++) tablinks[i].classList.remove("active");
    document.getElementById(tabName).style.display = "block";
    evt.currentTarget.classList.add("active");
  }
</script>
'
##
html_code <- glue("
<!DOCTYPE html>
<html lang=\"ko\">
<head>
  <meta charset=\"UTF-8\">
  <title>CTR / CVR 상승 상품 리포트</title>
  <style>
    body {{
      font-family: sans-serif;
      margin: 2em;
    }}
    .tab {{
      overflow: hidden;
      border-bottom: 1px solid #ccc;
    }}
    .tab button {{
      background-color: inherit;
      border: none;
      outline: none;
      cursor: pointer;
      padding: 10px 20px;
      transition: 0.3s;
      font-size: 16px;
    }}
    .tab button:hover {{
      background-color: #ddd;
    }}
    .tab button.active {{
      background-color: #ccc;
    }}
    .tabcontent {{
      display: none;
      padding: 20px 0;
    }}
    .tabcontent img {{
      max-width: 95%;
      margin-bottom: 1em;
    }}
    .tabcontent h2 {{
      margin-top: 0;
    }}
    #auth-box {{
      text-align: center;
      margin-top: 100px;
    }}
  </style>
</head>
<body>

<div id=\"auth-box\">
  <h2>비밀번호를 입력하세요</h2>
  <input type=\"password\" id=\"pw\" placeholder=\"비밀번호 입력\" />
  <button onclick=\"checkPassword()\">확인</button>
</div>

<div id=\"main-content\" style=\"display: none;\">
  <div class=\"tab\">
    <button class=\"tablinks\" onclick='openTab(event, \"report\")' id=\"defaultOpen\">CTR / CVR 상승 상품</button>
  </div>

  <div id=\"report\" class=\"tabcontent\">
    <h2>CTR / CVR 상승 상품 리포트</h2>
    <p>{today_label} 기준 최근 7일 대비 그 이전 7일동안 CTR/CVR이 상승한 상위 10개 상품입니다.</p>
    <div style=\"margin-bottom: 16px; font-size: 14px; line-height: 1.5;\">
      <strong>📌 지표 설명</strong><br>
      - <strong>최근 7일 평균 판매량</strong>: 최근 7일간 평균 판매수<br>
      - <strong>이전 7일 평균 판매량</strong>: 이전 7일간 평균 판매수<br>
      - <strong>CTR 상승률 (%)</strong>: 이전 7일 대비 CTR(클릭수/노출수) 상승률 (%)<br>
      - <strong>CVR 상승률 (%)</strong>: 이전 7일 대비 CVR(판매수/노출수) 상승률 (%)<br>
      - <strong>판매 순위</strong>: 최근 7일 판매량 기준 순위 (이전 7일 순위)
    </div>
    <img src=\"summary_7day_compare_table.png\" alt=\"Top30 상품 표\">
    <img src=\"summary_rank_change_top10_table.png\" alt=\"Top10 상품 표\">
    <img src=\"rank_change_top10_trend.png\" alt=\"Top10 상품 추이 그래프\">
  </div>
</div>

<script src=\"https://cdn.jsdelivr.net/npm/js-sha256@0.9.0/src/sha256.min.js\"></script>
<script>
  const HASHED_PASSWORD = \"none\";

  function checkPassword() {{
    const input = document.getElementById(\"pw\").value.trim();
    if (input === \"dr_11610\") {{
      document.getElementById(\"auth-box\").style.display = \"none\";
      document.getElementById(\"main-content\").style.display = \"block\";
      document.getElementById(\"defaultOpen\")?.click();
    }} else {{
      alert(\"비밀번호가 틀렸습니다.\");
    }}
  }}

  function openTab(evt, tabName) {{
    const tabcontent = document.getElementsByClassName(\"tabcontent\");
    const tablinks = document.getElementsByClassName(\"tablinks\");
    for (let i = 0; i < tabcontent.length; i++) tabcontent[i].style.display = \"none\";
    for (let i = 0; i < tablinks.length; i++) tablinks[i].classList.remove(\"active\");
    document.getElementById(tabName).style.display = \"block\";
    evt.currentTarget.classList.add(\"active\");
  }}
</script>

</body>
</html>
")

writeLines(html_code, "index.html")

##
files_to_add <- c(
  "index.html",
  "summary_7day_compare_table.png",
  "summary_rank_change_top10_table.png",
  "rank_change_top10_trend.png"
)

# 존재하는 파일만 add
existing_files <- files_to_add[file.exists(files_to_add)]
system(glue("git add {paste(existing_files, collapse=' ')}"), intern = TRUE)


# 🚀 Git 강제 Push (충돌 무시)
system("git config user.name 'github-actions'")
system("git config user.email 'actions@github.com'")
system("git add index.html summary_7day_compare_table.png summary_rank_change_top10_table.png rank_change_top10_trend.png", intern = TRUE)
commit_log <- system("git commit -m '자동 리포트 갱신' || echo 'No changes to commit'", intern = TRUE)

if (!any(grepl("No changes to commit", commit_log))) {
  cat("✅ 커밋 완료 → 강제 push 실행\n")
  system("git push origin main --force", intern = TRUE)
  cat("✅ 강제 push 완료\n")
} else {
  cat("ℹ️ 변경사항 없음. Git push 생략됨.\n")
}
