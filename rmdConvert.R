rmd.convert <- function(fname, output=c('latex', 'word', "pdf")){
  ## Thanks to Robert Musk for helpful additions to make this run better on Windows
  
  require(knitr)
  require(tools)
  
  thedir <- file_path_as_absolute(dirname(fname))
  thefile <- (basename(fname)) 
  
  create_latex <- function(f){
    knit(f, 'tmp-outputfile.md', encoding = "UTF-8"); 
    newname <- paste0(file_path_sans_ext(f), ".tex")
    mess <- paste('pandoc -f markdown -t latex -s -o', shQuote(newname), 
                  "tmp-outputfile.md")
    system(mess)
    cat("The Latex file is", file.path(thedir, newname), 
        "\nIf transporting do not forget to include the folder", file.path(thedir, "figure"), "\n")
    file.remove('tmp-outputfile.md')
  }
  
  create_word <- function(f){
    knit(f, 'tmp-outputfile.md', encoding = "UTF-8");
    newname <- paste0(file_path_sans_ext(f),".docx")
    mess <- paste('pandoc -f markdown -t docx -o', shQuote(newname), "tmp-outputfile.md")
    system(mess)
    cat("The Word (docx) file is", file.path(thedir, newname), "\n")
    file.remove('tmp-outputfile.md')
  }
  
  create_pdf <- function(f){
    knit(f, 'tmp-outputfile.md', encoding = "UTF-8");
    newname <- paste0(file_path_sans_ext(f),".pdf")
    mess <- paste('pandoc -f markdown -o', shQuote(newname), "tmp-outputfile.md")
    system(mess)
    cat("The PDF file is", file.path(thedir, newname), "\n")
    file.remove('tmp-outputfile.md')
  }
  
  origdir <- getwd()  
  tryCatch({
    setwd(thedir) ## put us next to the original Rmarkdown file
    out <- match.arg(output)
    switch(out,
           latex=create_latex(thefile),
           pdf=create_pdf(thefile),
           word=create_word(thefile)
    )}, finally=setwd(origdir))
  
}