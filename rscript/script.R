################
#
# rate den deliminator in csv files
#
################




#####
#
# testdaten kreator
#
#####

# daten <- data.frame(
#     id = 1:10,
#     name = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"),
#     wert = runif(10),
#     text = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
#     )
# write.csv(daten, "../testdata/vorlage.csv", row.names=FALSE)





#####
#
# lade testdaten
#
#####
# komma <- read.csv("../testdata/komma.csv")
# tabulator <- read.csv("../testdata/tabulator.csv")
# semikolon <- read.csv("../testdata/semikolon.csv")
# komma_badFormat <- read.csv("../testdata/komma_badFormat.csv")
# komma_badFormat2 <- read.csv("../testdata/komma_badFormat2.csv")



testall <- function() {
    return(
        guessSep("../testdata/komma.csv") == "," &
        guessSep("../testdata/tabulator.csv") == "\t" &
        guessSep("../testdata/semikolon.csv") == ";" &
        guessSep("../testdata/komma_badFormat.csv") == ",")

}

#####
#
# funktion
#
##### 
guessSep <- function(filepath) {
    FILEPATH = filepath

    #####
    #
    # zu testende seperatoren
    #
    #####

    seperators <- list(
        komma=",",
        semikolon = ";",
        tabulator = "\t")


    #####
    #
    # hilfsfunktionen
    #
    #####

    read_n_lines_from_file_with_sep <- function(
        filename, nlines, sep) {
        return(read.csv(filename, nrows=nlines, sep = sep))
    }

    read_100_lines_with_sep <- function(
        filename, sep) {
        return(read_n_lines_from_file_with_sep(filename, 100, sep))
    }

    estimate_colcount <- function(import) {
        return(ncol(import))
    }


    find_biggest <- function(liste_von_colcounts) {
        liste_von_colcounts[which.max(liste_von_colcounts)]
    }

    find_biggest_name <- function(listeneintrag) {
        names(find_biggest(listeneintrag))
    }


    read_file_with_sep <- function(sep) {
        return( estimate_colcount(read_100_lines_with_sep(
            FILEPATH, sep)))
    }


    #####
    #
    # lapply ueber seperatoren liste
    #
    #####

    get_list_of_rowestimates <- function() {
        lapply(seperators, read_file_with_sep)
    }


    #####
    #
    # gibt [[1]] 'value' element zurueck
    # (find_biggest_name gibt named ding ..)
    #
    #####

    return(seperators[find_biggest_name(
        get_list_of_rowestimates())][[1]])
}