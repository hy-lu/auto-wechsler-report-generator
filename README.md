# 韦氏智力测验报告自动生成器

仅需要原始的 PDF 报告就能产生相应的 Word 版报告！

## 使用方法

1. 将在网上录入分数后产生的 PDF 报告放入 `source` 文件夹内（可以同时放入多个不同年龄段的幼儿或儿童版）
2. 返回根目录，双击 `runWechsler.bat`
3. 成功生成的 Word 文件可以在 `output` 文件夹中找到

## 前期准备

- 需要将 `R.exe`/`Rscript.exe` 所在路径（Windows 64位系统一般为 `C:\Program Files\R\R-3.4.4\bin\x64`）[加入系统环境变量中](https://tinyurl.com/y9rjmgxe)
- 需要安装一些 R packages。不过如果缺少必须的包的话，在第一次运行时会自动安装相应的包

## 其它

- 可以在 `template` 文件夹中找到 `.rmd` 和 `.docx` 的模板，前者用于控制内容（其中目前包含了两种动态产生 Markdown 文本的方法），后者用于控制产生的 Word 报告中标题、正文、图片等内容的格式
- `template/temp` 文件夹会保存从每个 PDF 报告中所截取出的图片，可以自己使用或删除

***

# Automatic report generator for Wechsler IQ test

It is a convenient tool for automatically generate Wechsler test report. What you need to do is save PDF reports in the `source` folder and click on `runWechsler.bat`, and you can find the corresponding Word docs in the `output` folder.
