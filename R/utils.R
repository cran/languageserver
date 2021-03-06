#' Merge two lists
#'
#' @keywords internal
merge_list <- function(x, y) {
    x[names(y)] <- y
    x
}

#' tryCatch with stack captured
#'
#' @keywords internal
tryCatchStack <- function(expr, ...) {
    expr <- substitute(expr)
    env <- parent.frame()
    capture_calls <- function(e) {
        calls <- sys.calls()
        ncalls <- length(calls)
        e$calls <- calls[-c(seq_len(frame + 7), ncalls - 1, ncalls)]
        class(e) <- c("errorWithStack", class(e))
        signalCondition(e)
    }
    frame <- sys.nframe()
    tryCatch(withCallingHandlers(eval(expr, env), error = capture_calls), ...)
}

print.errorWithStack <- function(x, ...) {
    cat("Error: ", conditionMessage(x), "\n", sep = "")

    call <- conditionCall(x)
    if (!is.null(call)) {
        cat("Call: ")
        print(call)
    }

    if (length(x$calls)) {
        cat("Stack trace:\n")
        rev_calls <- rev(x$calls)
        for (i in seq_along(rev_calls)) {
            cat(i, ": ", sep = "")
            print(rev_calls[[i]])
        }
    }
    invisible(x)
}

tryCatchTimeout <- function(expr, timeout = Inf, ...) {
    expr <- substitute(expr)
    envir <- parent.frame()
    setTimeLimit(timeout, transient = TRUE)
    on.exit(setTimeLimit())
    tryCatch(eval(expr, envir), ...)
}

capture_print <- function(x) {
    paste0(utils::capture.output(print(x)), collapse = "\n")
}

get_expr_type <- function(expr) {
    if (is.call(expr)) {
        func <- deparse(expr[[1]], nlines = 1)
        if (func == "function") {
            "function"
        } else if (func %in% c("c", "matrix", "array")) {
            "array"
        } else if (func == "list") {
            "list"
        } else if (grepl("(R6:::?)?R6Class", func)) {
            "class"
        } else {
            "variable"
        }
    } else {
        typeof(expr)
    }
}

uri_escape_unicode <- function(uri) {
    if (length(uri) == 0) {
        return(character())
    }
    if (.Platform$OS.type == "windows") {
        uri <- utils::URLdecode(uri)
        Encoding(uri) <- "UTF-8"
        utils::URLencode(uri)
    } else {
        utils::URLencode(uri)
    }
}

#' Paths and uris
#' @keywords internal
path_from_uri <- function(uri) {
    if (length(uri) == 0) {
        return(character())
    }
    if (!startsWith(uri, "file:///")) {
        return("")
    }
    # URLdecode gives unknown encoding, we need to mark them as UTF-8
    start_char <- if (.Platform$OS.type == "windows") 9 else 8
    path <- utils::URLdecode(substr(uri, start_char, nchar(uri)))
    Encoding(path) <- "UTF-8"
    path
}

#' @keywords internal
#' @rdname path_from_uri
path_to_uri <- function(path) {
    if (length(path) == 0) {
        return(character())
    }
    path <- path.expand(path)
    if (.Platform$OS.type == "windows") {
        prefix <- "file:///"
        path <- gsub("\\", "/", path, fixed = TRUE)
    } else {
        prefix <- "file://"
    }
    paste0(prefix, utils::URLencode(path))
}


path_has_parent <- function(x, y) {
    if (.Platform$OS.type == "windows") {
        tryCatch(
            # https://github.com/REditorSupport/languageserver/issues/279
            fs::path_has_parent(x, y),
            error = function(e) {
                # try encode to native encoding
                fs::path_has_parent(enc2native(x), enc2native(y))
            }
        )
    } else {
        fs::path_has_parent(x, y)
    }
}

equal_position <- function(x, y) {
    x$line == y$line && x$character == y$character
}

equal_range <- function(x, y) {
    equal_position(x$start, y$start) && equal_position(x$end, y$end)
}

equal_definition <- function(x, y) {
    x$uri == y$uri && equal_range(x$range, y$range)
}

#' Check if a file is an RMarkdown file
#' @keywords internal
is_rmarkdown <- function(uri) {
    filename <- path_from_uri(uri)
    endsWith(tolower(filename), ".rmd") || endsWith(tolower(filename), ".rmarkdown")
}

#' Check if a token is in a R code block in an Rmarkdown file
#'
#' In an RMarkdown document, tokens can be either inside an R code block or
#' in the text. This function will return `FALSE` if the token is in the text
#' and `TRUE` if it is in a code block. For any other files, it always returns `TRUE`.
#'
#' @keywords internal
check_scope <- function(uri, document, point) {
    if (is_rmarkdown(uri)) {
        row <- point$row
        flags <- vapply(
            document$content[1:(row + 1)], startsWith, logical(1), "```", USE.NAMES = FALSE)
        if (any(flags)) {
            last_match <- document$content[max(which(flags))]
            stringi::stri_detect_regex(last_match, "```+\\s*\\{[rR][ ,\\}]") &&
                !identical(sum(flags) %% 2, 0) &&
                !enclosed_by_quotes(document, point)
        } else {
            FALSE
        }
    } else {
        !enclosed_by_quotes(document, point)
    }
}

match_with <- function(x, token) {
    pattern <- gsub(".", "\\.", token, fixed = TRUE)
    grepl(pattern, x, ignore.case = TRUE)
}

fuzzy_find <- function(x, pattern) {
    subsequence_regex <- gsub("(.)", "\\1.*", pattern)
    grepl(subsequence_regex, x, ignore.case = TRUE)
}


#' Safer version of `seq` which returns empty vector if b < a
#' @keywords internal
seq_safe <- function(a, b) {
    seq(a, b, length = max(0, b - a + 1))
}


#' Extract the R code blocks of a Rmarkdown file
#' @keywords internal
extract_blocks <- function(content) {
    begins_or_ends <- which(stringi::stri_detect_fixed(content, "```"))
    begins <- which(stringi::stri_detect_regex(content, "```+\\s*\\{[rR][ ,\\}]"))
    ends <- setdiff(begins_or_ends, begins)
    blocks <- list()
    for (begin in begins) {
        z <- which(ends > begin)
        if (length(z) == 0) break
        end <- ends[min(z)]
        lines <- seq_safe(begin + 1, end - 1)
        if (length(lines) > 0) {
            blocks[[length(blocks) + 1]] <- list(lines = lines, text = content[lines])
        }
    }
    blocks
}

get_signature <- function(symbol, expr) {
    signature <- format(expr[1:2])
    signature <- paste0(trimws(signature, which = "left"), collapse = "")
    signature <- gsub("^function\\s*", symbol, signature)
    signature <- gsub("\\s*NULL$", "", signature)
    signature
}

get_r_document_sections <- function(uri, document, type = c("section", "subsections")) {
    sections <- NULL

    if ("section" %in% type) {
        section_lines <- seq_len(document$nline)
        section_lines <- section_lines[
            grep("^\\#+\\s*(.+?)\\s*(\\#{4,}|\\+{4,}|\\-{4,}|\\={4,})\\s*$",
                document$content[section_lines], perl = TRUE)]
        if (length(section_lines)) {
            section_names <- gsub("^\\#+\\s*(.+?)\\s*(\\#{4,}|\\+{4,}|\\-{4,}|\\={4,})\\s*$",
                "\\1", document$content[section_lines], perl = TRUE)
            section_end_lines <- c(section_lines[-1] - 1, document$nline)
            sections <- .mapply(function(name, start_line, end_line) {
                list(
                    name = name,
                    type = "section",
                    start_line = start_line,
                    end_line = end_line
                )
            }, list(section_names, section_lines, section_end_lines), NULL)
        }
    }

    subsections <- NULL

    if ("subsection" %in% type) {
        subsection_lines <- seq_len(document$nline)
        subsection_lines <- subsection_lines[
            grep("^\\s+\\#+\\s*(.+?)\\s*(\\#{4,}|\\+{4,}|\\-{4,}|\\={4,})\\s*$",
                document$content[subsection_lines], perl = TRUE)]
        if (length(subsection_lines)) {
            subsection_names <- gsub("^\\s+\\#+\\s*(.+?)\\s*(\\#{4,}|\\+{4,}|\\-{4,}|\\={4,})\\s*$",
                "\\1", document$content[subsection_lines], perl = TRUE)
            subsections <- .mapply(function(name, line) {
                list(
                    name = name,
                    type = "subsection",
                    start_line = line,
                    end_line = line
                )
            }, list(subsection_names, subsection_lines), NULL)
        }
    }

    c(sections, subsections)
}

get_rmd_document_sections <- function(uri, document, type = c("section", "chunk")) {
    content <- document$content
    if (length(content) == 0) {
        return(NULL)
    }

    block_lines <- grep("^\\s*```", content)
    if (length(block_lines) %% 2 != 0) {
        return(NULL)
    }

    sections <- NULL
    if ("section" %in% type) {
        section_lines <- grepl("^#+\\s+\\S+", content)
        if (grepl("^---\\s*$", content[[1]])) {
            front_start <- 1L
            front_end <- 2L
            while (front_end <= document$nline) {
                if (grepl("^---\\s*$", content[[front_end]])) {
                    break
                }
                front_end <- front_end + 1L
            }
            section_lines[seq.int(front_start, front_end)] <- FALSE
        }

        for (i in seq_len(length(block_lines) / 2)) {
            section_lines[seq.int(block_lines[[2 * i - 1]], block_lines[[2 * i]])] <- FALSE
        }

        section_lines <- which(section_lines)
        section_num <- length(section_lines)
        section_texts <- content[section_lines]
        section_hashes <- gsub("^(#+)\\s+.+$", "\\1", section_texts)
        section_levels <- nchar(section_hashes)
        section_names <- gsub("^#+\\s+(.+?)(\\s+#+)?\\s*$", "\\1", section_texts, perl = TRUE)

        sections <- lapply(seq_len(section_num), function(i) {
            start_line <- section_lines[[i]]
            end_line <- document$nline
            level <- section_levels[[i]]
            j <- i + 1
            while (j <= section_num) {
                if (section_levels[[j]] <= level) {
                    end_line <- section_lines[[j]] - 1
                    break
                }
                j <- j + 1
            }
            list(
                name = section_names[[i]],
                type = "section",
                start_line = start_line,
                end_line = end_line
            )
        })
    }

    chunks <- NULL
    if ("chunk" %in% type) {
        unnamed_chunks <- 0
        chunks <- lapply(seq_len(length(block_lines) / 2), function(i) {
            start_line <- block_lines[[2 * i - 1]]
            end_line <- block_lines[[2 * i]]
            label <- stringi::stri_match_first_regex(content[[start_line]],
                "^\\s*```+\\s*\\{[a-zA-Z0-9_]+\\s*(([^,='\"]+)|'(.+)'|\"(.+)\")\\s*(,.+)?\\}\\s*$"
            )[1, 3:5]
            name <- label[!is.na(label)]

            if (length(name) == 0) {
                unnamed_chunks <<- unnamed_chunks + 1
                name <- sprintf("unnamed-chunk-%d", unnamed_chunks)
            }

            list(
                name = name,
                type = "chunk",
                start_line = start_line,
                end_line = end_line
            )
        })
    }

    c(sections, chunks)
}

get_document_sections <- function(uri, document,
    type = c("section", "subsection", "chunk")) {
    if (document$is_rmarkdown) {
        get_rmd_document_sections(uri, document, type)
    } else {
        get_r_document_sections(uri, document, type)
    }
}

#' Strip out all the non R blocks in a R markdown file
#' @param content a character vector
#' @keywords internal
purl <- function(content) {
    blocks <- extract_blocks(content)
    rmd_content <- rep("", length(content))
    for (block in blocks) {
        rmd_content[block$lines] <- content[block$lines]
    }
    rmd_content
}


#' Calculate character offset based on the protocol
#' @keywords internal
ncodeunit <- function(s) {
    lengths(iconv(s, from = "UTF-8", to = "UTF-16BE", toRaw = TRUE)) / 2
}


#' Determine code points given code units
#'
#' @param line a character of text
#' @param units 0-indexed code points
#'
#' @keywords internal
code_point_from_unit <- function(line, units) {
    if (!nzchar(line)) return(units)
    offsets <- cumsum(ncodeunit(strsplit(line, "")[[1]]))
    loc_map <- match(seq_len(utils::tail(offsets, 1)), offsets)
    result <- c(0, loc_map)[units + 1]
    n <- nchar(line)
    result[units > length(loc_map)] <- n
    result[is.infinite(units)] <- n
    result
}

#' Determine code units given code points
#'
#' @param line a character of text
#' @param units 0-indexed code units
#'
#' @keywords internal
code_point_to_unit <- function(line, pts) {
    if (!nzchar(line)) return(pts)
    offsets <- c(0, cumsum(ncodeunit(strsplit(line, "")[[1]])))
    result <- offsets[pts + 1]
    n <- length(offsets)
    m <- offsets[n]
    result[pts >= n] <- m
    result[is.infinite(pts)] <- m
    result
}


#' Check if a path is a directory
#' @keywords internal
is_directory <- function(path) {
    is_dir <- file.info(path)$isdir
    !is.na(is_dir) && is_dir
}

#' Find the root package folder
#'
#' This function searches backwards in the folder structure until it finds
#' a DESCRIPTION file or it reaches the top-level directory.
#'
#' @keywords internal
find_package <- function(path = getwd()) {
    start_path <- getwd()
    on.exit(setwd(start_path))
    if (!file.exists(path)) {
        return(NULL)
    }
    setwd(path)
    prev_path <- ""
    while (!file.exists(file.path(prev_path, "DESCRIPTION"))) {
        if (identical(prev_path, getwd())) {
            return(NULL)
        }
        prev_path <- getwd()
        setwd("..")
    }
    normalizePath(prev_path)
}

#' check if a path is a package folder
#'
#' @param rootPath a character representing a path
#'
#' @keywords internal
is_package <- function(rootPath) {
    file <- file.path(rootPath, "DESCRIPTION")
    file.exists(file) && !dir.exists(file)
}

#' read a character from stdin
#'
#' @keywords internal
stdin_read_char <- function(n) {
    .Call("stdin_read_char", PACKAGE = "languageserver", n)
}

#' read a line from stdin
#'
#' @keywords internal
stdin_read_line <- function() {
    .Call("stdin_read_line", PACKAGE = "languageserver")
}

#' check if the current process becomes an orphan
#'
#' @keywords internal
process_is_detached <- function() {
    .Call("process_is_detached", PACKAGE = "languageserver")
}

#' throttle a function execution
#'
#' Execute a function if the last execution time is older than a specified
#' value.
#'
#' @param fun the function to execute
#' @param t an integer, the threshold in seconds
#'
#' @keywords internal
throttle <- function(fun, t = 1) {
    last_execution_time <- 0
    function(...) {
        if (Sys.time() - last_execution_time > t) {
            last_execution_time <<- Sys.time()
            fun(...)
        }
    }
}

#' sanitize package objects names
#'
#' Remove unwanted objects, _e.g._ `names<-`, `%>%`, `.__C_` etc.
#'
#' @keywords internal
sanitize_names <- function(objects) {
    objects[stringi::stri_detect_regex(objects, "^([^\\W_]|\\.(?!_))(\\w|\\.)*$")]
}

na_to_empty_string <- function(x) if (is.na(x)) "" else x
empty_string_to_null <- function(x) if (nzchar(x)) x else NULL

look_forward <- function(text) {
    matches <- stringi::stri_match_first_regex(text, "^(?:[^\\W]|\\.)*\\b")[1]
    list(
        token = na_to_empty_string(matches[1])
    )
}

look_backward <- function(text) {
    matches <- stringi::stri_match_first_regex(
        text, "(?<!\\$)(?:\\b|(?=\\.))(?:([a-zA-Z][a-zA-Z0-9.]+)(:::?))?((?:[^\\W_]|\\.)(?:[^\\W]|\\.)*)?$")
    list(
        full_token = na_to_empty_string(matches[1]),
        package  = na_to_empty_string(matches[2]),
        accessor = na_to_empty_string(matches[3]),
        token = na_to_empty_string(matches[4])
    )
}

str_trunc <- function(string, width, ellipsis = "...") {
    trunc <- !is.na(string) && nchar(string) > width
    if (trunc) {
        width2 <- width - nchar(ellipsis)
        paste0(substr(string, 1, width2), ellipsis)
    } else {
        string
    }
}

uncomment <- function(x) gsub("^\\s*#+'?\\s*", "", x)

convert_comment_to_documentation <- function(comment) {
    result <- NULL
    roxy <- tryCatch(roxygen2::parse_text(c(
        comment,
        "NULL"
    ), env = NULL), error = function(e) NULL)
    if (length(roxy)) {
        result <- list(
            title = NULL,
            description = NULL,
            arguments = list(),
            markdown = NULL
        )
        items <- lapply(roxy[[1]]$tags, function(item) {
            if (item$tag == "title") {
                result$title <<- item$val
            } else if (item$tag == "description") {
                result$description <<- item$val
            } else if (item$tag == "param") {
                result$arguments[[item$val$name]] <<- item$val$description
            }

            format_roxy_tag(item)
        })
        if (is.null(result$description)) {
            result$description <- result$title
        }
        result$markdown <- paste0(items, collapse = "\n")
    }
    if (is.null(result)) {
        result <- paste0(uncomment(comment), collapse = "  \n")
    }
    result
}

format_roxy_tag <- function(item) {
    if (item$tag %in% c("title", "description")) {
        tag <- ""
        content <- format_roxy_text(item$val)
    } else {
        tag <- sprintf("`@%s` ", item$tag)
        content <- if (item$tag %in% c("usage", "example", "examples", "eval", "evalRd")) {
            sprintf("\n```r\n%s\n```", trimws(item$raw))
        } else if (is.character(item$val) && length(item$val) > 1) {
            paste0(sprintf("`%s`", item$val), collapse = " ")
        } else if (is.list(item$val)) {
            sprintf("`%s` %s", item$val$name, format_roxy_text(item$val$description))
        } else {
            format_roxy_text(item$raw)
        }
    }
    paste0(tag, content, "  \n")
}

format_roxy_text <- function(text) {
    gsub("\n", "  \n", text, fixed = TRUE)
}

find_doc_item <- function(doc, tag) {
    for (item in doc) {
        if (attr(item, "Rd_tag") == tag) {
            return(item)
        }
    }
}

convert_doc_to_markdown <- function(doc) {
    unlist(lapply(doc, function(item) {
        tag <- attr(item, "Rd_tag")
        if (is.null(tag)) {
            if (length(item)) {
                convert_doc_to_markdown(item)
            }
        } else if (tag == "\\R") {
            "**R**"
        } else if (tag == "\\dots") {
            "..."
        } else if (tag %in% c("\\code", "\\env", "\\eqn")) {
            sprintf("`%s`", paste0(convert_doc_to_markdown(item), collapse = ""))
        } else if (tag %in% c("\\ifelse", "USERMACRO")) {
            ""
        } else if (is.character(item)) {
            trimws(item)
        } else if (length(item)) {
            convert_doc_to_markdown(item)
        }
    }))
}

convert_doc_string <- function(doc) {
    paste0(convert_doc_to_markdown(doc), collapse = " ")
}

glue <- function(.x, ...) {
    param <- list(...)
    for (key in names(param)) {
        .x <- gsub(paste0("{", key, "}"), param[[key]], .x, fixed = TRUE)
    }
    .x
}

xdoc_find_enclosing_scopes <- function(x, line, col, top = FALSE) {
    if (top) {
        xpath <- "/exprlist | //expr[(@line1 < {line} or (@line1 = {line} and @col1 <= {col})) and
                (@line2 > {line} or (@line2 = {line} and @col2 >= {col}-1))]"
    } else {
        xpath <- "//expr[(@line1 < {line} or (@line1 = {line} and @col1 <= {col})) and
                (@line2 > {line} or (@line2 = {line} and @col2 >= {col}-1))]"
    }
    xpath <- glue(xpath, line = line, col = col)
    xml_find_all(x, xpath)
}

xdoc_find_token <- function(x, line, col) {
    xpath <- glue("//*[not(*)][(@line1 < {line} or (@line1 = {line} and @col1 <= {col})) and (@line2 > {line} or (@line2 = {line} and @col2 >= {col}-1))]",
        line = line, col = col)
    xml_find_first(x, xpath)
}

xml_single_quote <- function(x) {
    x <- gsub("'", "&apos;", x, fixed = TRUE)
    x
}

html_to_markdown <- function(html) {
    html_file <- file.path(tempdir(), "temp.html")
    md_file <- file.path(tempdir(), "temp.md")
    logger$info("Converting html to markdown using", html_file, md_file)
    stringi::stri_write_lines(html, html_file)
    result <- tryCatch({
        format <- if (rmarkdown::pandoc_version() >= "2.0") "gfm" else "markdown_github"
        rmarkdown::pandoc_convert(html_file, to = format, output = md_file)
        paste0(stringi::stri_read_lines(md_file, encoding = "utf-8"), collapse = "\n")
    }, error = function(e) {
        logger$info("html_to_markdown failed: ", conditionMessage(e))
        NULL
    })
    result
}

format_file_size <- function(bytes) {
    obj_size <- structure(bytes, class = "object_size")
    format(obj_size, units = "auto")
}

is_text_file <- function(path, n = 1000) {
    bin <- readBin(path, "raw", n = n)
    is_utf8 <- stringi::stri_enc_isutf8(bin)
    if (is_utf8) {
        return(TRUE)
    } else {
        result <- stringi::stri_enc_detect(bin)[[1]]
        conf <- result$Confidence[1]
        if (identical(conf, 1)) {
            return(TRUE)
        }
    }
    return(FALSE)
}
