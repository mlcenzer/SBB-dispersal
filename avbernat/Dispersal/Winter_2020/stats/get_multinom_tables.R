
rm(list=ls())
dir = "~/Desktop/git_repositories/SBB-dispersal/avbernat/Dispersal/Winter_2020/stats/"
setwd(dir) 

# tables
library(knitr)
library(kableExtra)


source_path = "~/Desktop/git_repositories/SBB-dispersal/avbernat/Rsrc/"

script_names = c("center_flight_data.R", # Re-centers data 
                 "clean_flight_data.R", # Loads and cleans data
                 "unique_flight_data.R"
                 )

for (script in script_names) { 
  path = paste0(source_path, script)
  source(path) 
}


data <- read_flight_data("data/all_flight_data-Winter2020.csv")
data_all <- data[[1]]
data_tested <- data[[2]]
d <- create_delta_data(data_tested)









file_outpath = "/Users/anastasiabernat/Desktop/git_repositories/SBB-dispersal/avbernat/Dispersal/Winter_2020/stats/tables/pt2-w2b-latex-odds.pdf"

old_caption = " * Instead of a 1% mass increase, which is relatively small change, the mass percent change estimates were multiplied by 20 before calculating the odds. This transformation better represents experimental observations and offers a more realistic odds."
caption = " * The mass percent change estimates and wing-to-body estimates were multiplied by 20 and 0.01, respectively, before calculating the odds. These transformations better represent experimental observations and offer a more realistic odds (e.g. 20% mass increase and a 0.01 unit increase in wing-to-body ratio)."

colname = "<span style='padding-right:30px'>Flew twice rather than did not fly         </span>"
colname = "Flew twice rather than did not fly "
colname ="Flew twice rather than did not fly <code style='background:white'>         </code>"
colnames(odds) = c("Variables in the Model", "Flew twice rather than flew in T1", "Flew twice rather than flew in T2", colname, "Flew in T1 rather than did not fly", "Flew in T2 rather than did not fly", "Flew in T1 rather than flew in T2")

align_cols = c("l", "c", "c", "c", "c", "c", "c") # align = align_cols

paper_table2 = kable(odds_s, table.attr = "style='width:87%;'", escape = FALSE) %>% # caption = top_caption
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))  %>%
  column_spec(4,latex_valign = "m")  %>%
  column_spec(1, width="4.5cm", background="white") %>%
  kable_classic(html_font = "Arial") %>%
  row_spec(seq(1,nrow(odds),2), background = "#f5f5f5") %>%
  footnote(general = caption, general_title = " ", number_title = "* " ) 

paper_table2



######################## fem table ########################

file_outpath = "/Users/anastasiabernat/Desktop/git_repositories/SBB-dispersal/avbernat/Dispersal/Winter_2020/stats/tables/fem-odds.pdf"

caption = " * The mass percent change estimates were multiplied by 20 before calculating the odds. This transformation better represents experimental observations and offers a more realistic odds (e.g. 20% mass increase)."

align_cols = c("l", "c", "c", "c", "c", "c", "c") # align = align_cols

paper_table2 = kable(oddsfinal, table.attr = "style='width:87%;'", escape = FALSE) %>% #caption = top_caption
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))  %>%
  #kbl(format="latex") %>%
  # column_spec(4,latex_valign = "m")  %>%
  column_spec(1, width="6cm", background="white") %>%
  # column_spec(4, width="3.5cm", background="white") %>%
  # column_spec(7, width="3.3cm", background="white") %>%
  # column_spec(6, width="3.3cm", background="white") %>%
  # column_spec(5, width="3.5cm", background="white") %>%
  # column_spec(3, width="3.3cm", background="white") %>%
  # column_spec(2, width="3.3cm", background="white") %>%
  kable_classic(html_font = "Arial") %>%
  row_spec(seq(1,nrow(odds),2), background = "#f5f5f5") %>%
  # column_spec(1, extra_css = "right-bottom: 1px solid") %>%
  # column_spec(1, color = "black", background = "white") %>%
  footnote(general = caption, general_title = " ", number_title = "* " ) 
# dat = odds_table %>%
#   kbl(format="latex") %>%
#   row_spec(2, bold=TRUE) %>%
#   save_kable(outpath)

# paper_table2 %>%
#   as_image(file=file_outpath)

# outpath = "/Users/anastasiabernat/Desktop/git_repositories/SBB-dispersal/avbernat/Dispersal/Winter_2020/stats/tables/"
# save_kable(paper_table2, file = paste0(outpath,"paper-table2-w2b-latex-odds.png"), keep_tex = TRUE, zoom = 5)

paper_table2