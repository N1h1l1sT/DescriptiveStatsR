.onLoad <- function(libname, pkgname) {
}

`%notin%` <- function(x, table) is.na(match(x, table, nomatch = NA_integer_))
is.not.null <- function(x)(!is.null(x))
is.not.na <- function(x)(!is.na(x))
False <- FALSE
True <- TRUE

#Changes the columns into rows and rows into columns for a DataFrame.
#Use: Transpose_DF(Dataframe)
#[Input: DataFrame] [Output: DataFrame]
Transpose_DF <- function(MyDF) {
  if (NROW(MyDF) > 0 && NCOL(MyDF) > 0) {
    TransposedDF <- data.table::transpose(MyDF)
    colnames(TransposedDF) <- rownames(MyDF)
    rownames(TransposedDF) <- colnames(MyDF)
    TransposedDF <- TransposedDF %>%
      tibble::rownames_to_column() %>%
      tibble::as_tibble()
  } else {
    TransposedDF <- MyDF
  }
  return(TransposedDF)
}

Right <- function(Text, n) {
  substring(Text, nchar(Text) - (n - 1))
}
Left <- function(Text, n) {
  substring(Text, 0, n)
}
Mid <- function(Text, StartIndex, Length) {
  substring(Text, StartIndex + 1, (StartIndex + Length))
}

CubeRoot <- function(Num) {
  return(sign(Num) * abs(Num)^(1/3))
}

#Get the Parent directory or a certain directory path
GetParentDir <- function(DirectoryPath) {
  paste0(dirname(DirectoryPath), "/")
}

#[input: String] [output: String()]
GetFileNames <- function(Directory) {
  dir(Directory, full.names = TRUE)
}

#[input: String or String()] [output: String or String()]
GetFileNamesAlone <- function(Directory) {
  basename(Directory)
}

#Get information as to what an object is
#[input: Object] [output: Printed Text]
#Incorporate more info depending on the VarType, like nchar for character variables !TODO
inspectVar <- function(x, BasicOnly = TRUE) {
  #deparse(substitute(x))
  print(data.frame(
    Typeof = implode(typeof(x), sep = ","),
    Class = implode(class(x), sep = ","),
    NRow = implode(NROW(x), sep = ","),
    NCol = implode(NCOL(x), sep = ","),
    Dim = ifelse(is.not.null(dim(x)), paste(dim(x), sep = ",", collapse = ","), "NULL")
  )
  )
  if (!BasicOnly) {
    print("structure:")
    print(str(x))
    print("summary:")
    print(summary(x))
  }

}
InspectVar <- function(x, BasicOnly = TRUE) inspectVar(x, BasicOnly)
