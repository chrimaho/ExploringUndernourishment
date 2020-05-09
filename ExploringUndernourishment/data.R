#------------------------------------------------------------------------------#
#                                                                              #
#    Title      : Get Data for Shiny                                           #
#    Purpose    : Import and Clean all the data.                               #
#    Notes      : .                                                            #
#    Author     : chrimaho                                                     #
#    Created    : 09/May/2020                                                  #
#    References : .                                                            #
#    Sources    : http://www.fao.org/faostat/en/#data/FS                       #
#    Edited     : 09/May/2020 - Initial creation                               #
#                                                                              #
#------------------------------------------------------------------------------#

# Import data ----
raw_DataPath <- find_rstudio_root_file("/ExploringUndernourishment/data/raw")
for (file in list.files(raw_DataPath, pattern="*.csv")) {
    filename <- str_remove(file, ".csv")
    assign(paste0("raw_",filename)
          ,read_csv(paste0(raw_DataPath, "/", file), col_types=cols())
          )
}

# Clean data ----
#GitSync()
