# TODO: add an option to avoid overwriting exsiting Word docs
# word_file <- list.files(path = "output/", pattern = ".\\.docx*", full.names = TRUE)

# TODO: improve the code flexibility for modularized application

# TODO: read test and birth dates using ocr from pdf

pkg_list <-
  c("pdftools",
    "tesseract",
    "magick",
    "stringr",
    "purrr",
    "readr",
    "dplyr",
    "glue",
    "rmarkdown")
if (!all(pkg_list %in% installed.packages())) {
  install.packages(setdiff(pkg_list, installed.packages()))
}

pdf_file <- list.files(path = "source/", pattern = ".\\.pdf", full.names = TRUE)
if (!dir.exists("output")) {
  dir.create("output")
} else if (!dir.exists("output/image")) {
  dir.create("output/image")
}

library(magick)
library(stringr)
library(purrr)
library(pdftools)
library(tesseract)

wechsler <- function(x) {
  scrshot <- magick::image_read_pdf(x, pages = 1, density = 144)
  full_test <- list(wppsi = "740x108+720+124",
                    wisc = "234x28+176+138") %>% 
    map(~{
      image_crop(scrshot, geometry = .x) %>% 
        image_ocr(language = "chi_sim") %>% 
        str_trim()
    }) %>% 
    str_subset("韦氏")
  version <- 
    image_crop(scrshot, geometry = "270x96+1102+370") %>% 
    image_resize("50%x") %>% 
    image_ocr(language = "chi_sim") %>% 
    str_trim()
  text <- pdftools::pdf_text(x)[1] %>%
    str_replace_all("\n+", "\n") %>%
    readr::read_lines() %>%
    stringr::str_trim() %>%
    stringr::str_split(pattern = "[:whitespace:]+")
  
  if (str_detect(version, "4.+6.+")) {
    stopifnot(length(text) %in% c(23, 24))
    sub_test <- "言语理解、视觉空间、流体推理、工作记忆和加工速度"
    name <- text[[2]][[1]]
    dates <- c(test = as.Date(paste(text[[1]][1:3], collapse = "/")),
               birth = as.Date(paste(text[[2]][2:4], collapse = "/")))
    fsiq <- as.numeric(str_replace(text[[23]][[2]], "无效", NA_character_))
    scores <- as.numeric(str_replace(c(text[[5]][-(1:3)]), "无效", NA_character_))
    stopifnot("At least one subscore is missing" = length(scores) == 11)
    names(scores) <-
      c("IN", "SI", "BD", "OA", "MR", "PC", "PM", "ZL", "BS", "CA", "AC")
    avg_scores <- c(
      `言语理解` = mean(scores[c("IN", "SI")], na.rm = T),
      `视觉空间` = mean(scores[c("BD", "OA")], na.rm = T),
      `流体推理` = mean(scores[c("MR", "PC")], na.rm = T),
      `工作记忆` = mean(scores[c("PM", "ZL")], na.rm = T),
      `加工速度` = mean(scores[c("BS", "CA", "AC")], na.rm = T)
    )
    scrshot_cropped <- image_crop(scrshot, "908x1010+1234+536")
  } else if (str_detect(version, "2.+3.+")) {
    stopifnot(length(text) %in% c(17,18))
    sub_test <- "言语理解、视觉空间和工作记忆"
    name <- text[[2]][[1]]
    dates <- c(test = as.Date(paste(text[[1]][1:3], collapse = "/")),
               birth = as.Date(paste(text[[2]][2:4], collapse = "/")))
    fsiq <- as.numeric(str_replace(text[[14]][[4]], "无效", NA_character_))
    scores <- as.numeric(str_replace(c(text[[4]][-1]), "无效", NA_character_))
    stopifnot("At least one subscore is missing" = length(scores) == 7)
    names(scores) <-
      c("RV", "IN", "PN", "BD", "OA", "PM", "ZL")
    avg_scores <- c(
      `言语理解` = mean(scores[c("RV", "IN", "PN")], na.rm = T),
      `视觉空间` = mean(scores[c("BD", "OA")], na.rm = T),
      `工作记忆` = mean(scores[c("PM", "ZL")], na.rm = T)
    )
    scrshot_cropped <- image_crop(scrshot, "889.04x1042.42+1255.22+451.92")
  } else if (str_detect(full_test, "儿童")) {
    stopifnot(length(text) %in% c(63))
    sub_test <- "言语理解、知觉推理、工作记忆和加工速度"
    name <- text[[9]][[2]]
    dates <-
      c(test = as.Date(paste(text[[16]][2:4], collapse = "/")),
        birth = as.Date(paste(text[[17]][2:4], collapse = "/")))
    fsiq <- as.numeric(str_replace(text[[40]][[3]], "无效", NA_character_))
    scores <- as.numeric(str_replace(text[[6]], "无效", NA_character_))
    stopifnot("At least one subscore is missing" = length(scores) == 6)
    names(scores) <-
      c("lei_tong", "li_jie", "ji_mu", "ju_zhen_tui_li", "bei_shu", "yi_ma")
    avg_scores <- c(
      `言语理解` = mean(scores[c("lei_tong", "li_jie")], na.rm = T),
      `知觉推理` = mean(scores[c("ji_mu", "ju_zhen_tui_li")], na.rm = T),
      `工作记忆` = mean(scores[c("bei_shu")], na.rm = T),
      `加工速度` = mean(scores[c("yi_ma")], na.rm = T)
    )
    scrshot_cropped <- image_crop(scrshot, "502.38x644.82+621.36+185.58")
  } else {
    simpleError(paste0(x, " seems like an invalid Wechsler Report PDF file!"))
  }
  
  good <- names(avg_scores[avg_scores > 10])
  avg <- names(avg_scores[avg_scores == 10])
  poor <- names(avg_scores[avg_scores < 10])

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
  i <- 2
  while (file.exists(paste0("output/",name, "_测评报告.docx"))) {
    name <- paste0(name,"(", i, ")")
    i <- i + 1
  }
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
