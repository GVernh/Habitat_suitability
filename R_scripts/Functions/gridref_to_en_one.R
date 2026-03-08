gridref_to_en_one <- function(gridref) {
  x <- toupper(gridref)
  x <- gsub("[^A-Z0-9]", "", x)
  
  if (!grepl("^[A-Z]{2}[0-9]+$", x)) return(c(E = NA_real_, N = NA_real_))
  
  let <- substr(x, 1, 2)
  dig <- substr(x, 3, nchar(x))
  
  if ((nchar(dig) %% 2) != 0) return(c(E = NA_real_, N = NA_real_))
  half <- nchar(dig) / 2
  
  e_str <- substr(dig, 1, half)
  n_str <- substr(dig, half + 1, nchar(dig))
  
  # letter -> 0..24 skipping I
  letter_to_num <- function(L) {
    n <- utf8ToInt(L) - utf8ToInt("A")
    if (n > (utf8ToInt("I") - utf8ToInt("A"))) n <- n - 1
    n
  }
  
  l1 <- letter_to_num(substr(let, 1, 1))
  l2 <- letter_to_num(substr(let, 2, 2))
  
  # 100km-grid indices (this is the important corrected bit)
  e100km <- ((l1 - 2) %% 5) * 5 + (l2 %% 5)
  n100km <- (19 - (l1 %/% 5) * 5) - (l2 %/% 5)
  
  # scale within 100km square (half=3 -> 100m; half=4 -> 10m; etc.)
  scale <- 10^(5 - half)
  
  e <- suppressWarnings(as.numeric(e_str)) * scale
  n <- suppressWarnings(as.numeric(n_str)) * scale
  
  c(E = e100km * 100000 + e,
    N = n100km * 100000 + n)
}