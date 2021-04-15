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
library(tesseract)

wechsler <- function(x) {
  ocr_engine <- tesseract(language = "chi_sim", 
                          options = list(tessedit_char_whitelist = "无效0123456789"))
  scrshot <- magick::image_read_pdf(x, pages = 1, density = 144)
  dates <- list(birth = NULL, test = NULL)
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
  
  if (str_detect(version, "4.+0.+6.+11")) {
    sub_test <- "言语理解、视觉空间、流体推理、工作记忆和加工速度"
    name <- image_crop(scrshot, geometry = "180.78x53.56+421.74+337.83") %>% 
      image_ocr(language = "chi_sim") %>% 
      str_trim()
    dates$test <- 
      c("78.78x39.52+1769.65+283.58",
        "78.78x39.52+1903.74+283.58",
        "78.78x39.52+2035.41+283.58") %>% 
      map_chr(~{image_crop(scrshot, .x) %>% 
      ocr(engine = ocr_engine) %>% 
      str_trim()}) %>% 
      str_c(collapse = "/")
    dates$birth <- 
      c("78.78x39.52+1769.65+350.36",
        "78.78x39.52+1903.74+350.36",
        "78.78x39.52+2035.41+350.36") %>% 
      map_chr(~{image_crop(scrshot, .x) %>% 
          ocr(engine = ocr_engine) %>% 
          str_trim()}) %>% 
      str_c(collapse = "/")
    fsiq <- image_crop(scrshot, geometry = "96x44+708+2238") %>% 
      image_ocr(language = "chi_sim") %>% 
      str_trim() %>% 
      as.numeric()
    scores <-
      paste("58.70x35.48", seq(664.11, by = 36.3, length.out = 11) * 2, 355.96 * 2, sep = "+") %>%
      map_dbl(~ {
        image_crop(scrshot, geometry = .x) %>%
          ocr(engine = ocr_engine) %>%
          str_trim() %>%
          str_replace("无效", NA_character_) %>%
          as.numeric()
      })
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
  } else if (str_detect(version, "2.+6.+3.+11")) {
    sub_test <- "言语理解、视觉空间和工作记忆"
    name <- image_crop(scrshot, geometry = "269.22x61.46+358.35+297.88") %>% 
      image_ocr(language = "chi_sim") %>% 
      str_trim()
    dates$test <- 
      c("83.35x30.52+1730.26+271.33",
        "83.35x30.52+1871.91+271.33",
        "83.35x30.52+2013.17+271.33") %>% 
      map_chr(~{image_crop(scrshot, .x) %>% 
          ocr(engine = ocr_engine) %>% 
          str_trim()}) %>% 
      str_c(collapse = "/")
    dates$birth <- 
      c("83.35x30.52+1730.26+334.42",
        "83.35x30.52+1871.91+324.86",
        "83.35x30.52+2013.17+324.86") %>% 
      map_chr(~{image_crop(scrshot, .x) %>% 
          ocr(engine = ocr_engine) %>% 
          str_trim()}) %>% 
      str_c(collapse = "/")
    fsiq <- image_crop(scrshot, geometry = "112x64+662+1944") %>% 
      image_ocr(language = "chi_sim") %>% 
      str_trim() %>% 
      as.numeric()
    scores <-
      paste("91.56x33.44", seq(675.74, by = 56.07, length.out = 7) * 2, 319.50 * 2, sep = "+") %>%
      map_dbl(~ {
        image_crop(scrshot, geometry = .x) %>%
          ocr(engine = ocr_engine) %>% 
          str_trim() %>%
          str_replace("无效", NA_character_) %>%
          as.numeric()
      })
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
    sub_test <- "言语理解、知觉推理、工作记忆和加工速度"
    name <- image_crop(scrshot, geometry = "81.98x20.74+249.57+310.41") %>% 
      image_ocr(language = "chi_sim") %>% 
      str_trim()
    dates$test <- 
      c("56.74x23.48+229.61+464.58",
        "56.74x23.48+352.48+464.58",
        "56.74x23.48+475.99+464.58") %>% 
      map_chr(~{image_crop(scrshot, .x) %>% 
          image_resize("150%x") %>%
          ocr(engine = ocr_engine) %>% 
          str_trim()}) %>% 
      str_c(collapse = "/")
    dates$birth <- 
      c("56.74x23.48+229.61+494.25",
        "56.74x23.48+352.48+494.25",
        "56.74x23.48+475.99+494.25") %>% 
      map_chr(~{image_crop(scrshot, .x) %>% 
          image_resize("150%x") %>%
          ocr(engine = ocr_engine) %>% 
          str_trim()}) %>% 
      str_c(collapse = "/")
    fsiq <- image_crop(scrshot, geometry = "48.52x18+1019.66+959.58") %>% 
      image_ocr(language = "chi_sim") %>% 
      str_trim() %>% 
      as.numeric()
    scores <-
      paste("34.42x17.22", seq(335.50, by = 38.86, length.out = 6) * 2, 136.22 * 2, sep = "+") %>%
      map_dbl(~ {
        image_crop(scrshot, geometry = .x) %>%
          ocr(engine = ocr_engine) %>% 
          str_trim() %>%
          str_replace("无效", NA_character_) %>%
          as.numeric()
      })
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
