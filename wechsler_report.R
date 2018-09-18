# TODO: add an option to avoid overwriting exsiting Word docs
# word_file <- list.files(path = "output/", pattern = ".\\.docx*", full.names = TRUE)

# TODO: improve the code flexibility for modularized application

pkg_list <-
  c("pdftools",
    "magick",
    "stringr",
    "magrittr",
    "tibble",
    "purrr",
    "readr",
    "dplyr",
    "rmarkdown")
if (!all(pkg_list %in% installed.packages())) {
  install.packages(setdiff(pkg_list, installed.packages()), dependencies = TRUE)
}
`%>%` <- magrittr::`%>%`
pdf_file <- list.files(path = "source/", pattern = ".\\.pdf", full.names = TRUE)
if (!dir.exists("output")) {
  dir.create("output")
} else if (!dir.exists("output/image")) {
  dir.create("output/image")
}

wechsler <- function(x) {
  text <- pdftools::pdf_text(x) %>%
    readr::read_lines() %>%
    stringr::str_trim() %>%
    stringr::str_split(pattern = "[:whitespace:]+")
  if (length(text) == 18) {
    full_test <- "韦氏幼儿智力测试(WPPSI-IV CN)"
    sub_test <- "言语理解、视觉空间和工作记忆"
    name <- text[[2]][[1]]
    dates <- c(test = as.Date(paste(text[[1]][1:3], collapse = "/")),
               birth = as.Date(paste(text[[2]][2:4], collapse = "/")))
    scores <- as.numeric(text[[14]][1:3])
    names(scores) <- c("言语理解", "视觉空间", "工作记忆")
    good <- names(scores[scores >= 110])
    avg <- names(scores[scores >= 90 & scores < 110])
    poor <- names(scores[scores < 90])
    fsiq <- as.numeric(text[[14]][4])

    scrshot <- magick::image_read_pdf(x, pages = 1, density = 72)
    scrshot_info <- magick::image_info(scrshot)
    scrshot_cropped <-
      magick::image_crop(
        scrshot,
        magick::geometry_area(
          width = scrshot_info$width * 0.4952,
          height = scrshot_info$height * 0.2220,
          x_off = scrshot_info$width * 0.0390,
          y_off = scrshot_info$height * 0.4732
        )
      )
  }
  if (length(text) == 23) {
    full_test <- "韦氏幼儿智力测试(WPPSI-IV CN)"
    sub_test <- "言语理解、视觉空间、流体推理、工作记忆和加工速度"
    name <- text[[2]][[1]]
    dates <- c(test = as.Date(paste(text[[1]][1:3], collapse = "/")),
               birth = as.Date(paste(text[[2]][2:4], collapse = "/")))
    scores <- as.numeric(c(text[[5]][-(1:3)]))
    names(scores) <-
      c("IN", "SI", "BD", "OA", "MR", "PC", "PM", "ZL", "BS", "CA", "AC")
    fsiq <- as.numeric(text[[23]][[2]])

    avg_scores <- c(
      `言语理解` = mean(scores[c("IN", "SI")], na.rm = T),
      `视觉空间` = mean(scores[c("BD", "OA")], na.rm = T),
      `流体推理` = mean(scores[c("MR", "PC")], na.rm = T),
      `工作记忆` = mean(scores[c("PM", "ZL")], na.rm = T),
      `加工速度` = mean(scores[c("BS", "CA", "AC")], na.rm = T)
    )
    good <- names(avg_scores[avg_scores > 10])
    avg <- names(avg_scores[avg_scores == 10])
    poor <- names(avg_scores[avg_scores < 10])
    scrshot <- magick::image_read_pdf(x, pages = 1, density = 72)
    scrshot_info <- magick::image_info(scrshot)
    scrshot_cropped <-
      magick::image_crop(
        scrshot,
        magick::geometry_area(
          width = scrshot_info$width * 0.405,
          height = scrshot_info$height * 0.35,
          x_off = scrshot_info$width * 0.54,
          y_off = scrshot_info$height * 0.18
        )
      )
  }
  if (length(text) == 63) {
    full_test <- "韦氏儿童智力测试(WISC-IV CN)"
    sub_test <- "言语理解、知觉推理、工作记忆和加工速度"
    name <- text[[9]][[2]]
    dates <-
      c(test = as.Date(paste(text[[16]][2:4], collapse = "/")),
        birth = as.Date(paste(text[[17]][2:4], collapse = "/")))
    scores <- as.numeric(text[[6]])
    names(scores) <- text[[5]]
    fsiq <- as.numeric(text[[40]][[3]])

    avg_scores <- c(mean(scores[c("类同", "理解")], na.rm = T),
                    mean(scores[c("积木", "矩阵推理")], na.rm = T),
                    scores["背数"],
                    scores["译码"])
    names(avg_scores) <- text[[4]]
    avg_scores <- avg_scores[!is.na(avg_scores)]
    good <- names(avg_scores[avg_scores > 10])
    avg <- names(avg_scores[avg_scores == 10])
    poor <- names(avg_scores[avg_scores < 10])
    scrshot <- magick::image_read_pdf(x, pages = 1, density = 144)
    scrshot_info <- magick::image_info(scrshot)
    scrshot_cropped <-
      magick::image_crop(
        scrshot,
        magick::geometry_area(
          width = scrshot_info$width * 0.42,
          height = scrshot_info$height * 0.38,
          x_off = scrshot_info$width * 0.52,
          y_off = scrshot_info$height * 0.11
        )
      )
  }

  fsiq_disp <- dplyr::case_when(
    fsiq < 90 ~ "中下水平",
    fsiq >= 90 & fsiq < 110 ~ "中等水平",
    fsiq >= 110 & fsiq < 120 ~ "中上水平",
    fsiq >= 120 ~ "优秀水平"
  )
  good_disp <-
    ifelse(length(good) > 0, paste0(paste(good, collapse = "、"), "上表现较好"), NA)
  avg_disp <-
    ifelse(length(avg) > 0, paste0(paste(avg, collapse = "、"), "上处于平均水平"), NA)
  poor_disp <-
    ifelse(length(poor) > 0, paste0(paste(poor, collapse = "、"), "上表现有待提高"), NA)
  disp <- c(good_disp, avg_disp, poor_disp)
  full_disp <- paste(disp[!is.na(disp)], collapse = "，")
  data_list <- list(
    name = name,
    dates = dates,
    full_disp = full_disp,
    fsiq = fsiq,
    fsiq_disp = fsiq_disp,
    full_test = full_test,
    sub_test = sub_test
  )
  magick::image_write(
    scrshot_cropped,
    path = paste0("output/image/", name, "_scrshot_cropped.png"),
    format = "png"
  )
  rmarkdown::render(
    input = "template/wechsler_template.rmd",
    output_file = paste0(name, "_测评报告.docx"),
    output_dir = "output/",
    params = list(
      data_list = data_list
    ),
    encoding = "UTF-8"
  )
}

lapply(pdf_file, wechsler)
