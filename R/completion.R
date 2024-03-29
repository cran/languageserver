# TODO: group the completions into different catagories according to
# https://github.com/wch/r-source/blob/trunk/src/library/utils/R/completion.R

CompletionItemKind <- list(
    Text = 1,
    Method = 2,
    Function = 3,
    Constructor = 4,
    Field = 5,
    Variable = 6,
    Class = 7,
    Interface = 8,
    Module = 9,
    Property = 10,
    Unit = 11,
    Value = 12,
    Enum = 13,
    Keyword = 14,
    Snippet = 15,
    Color = 16,
    File = 17,
    Reference = 18,
    Folder = 19,
    EnumMember = 20,
    Constant = 21,
    Struct = 22,
    Event = 23,
    Operator = 24,
    TypeParameter = 25
)

InsertTextFormat <- list(
    PlainText = 1,
    Snippet = 2
)

sort_prefixes <- list(
    arg = "0-",
    scope = "1-",
    workspace = "2-",
    imported = "3-",
    global = "4-",
    token = "5-"
)

constants <- c("TRUE", "FALSE", "NULL",
    "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_",
    "Inf", "NaN")

#' Complete language constants
#' @noRd
constant_completion <- function(token) {
    consts <- constants[match_with(constants, token)]
    completions <- lapply(consts, function(const) {
        list(label = const,
            kind = CompletionItemKind$Constant,
            sortText = paste0(sort_prefixes$global, const),
            data = list(type = "constant")
        )
    })
}

#' Complete a package name
#' @noRd
package_completion <- function(token) {
    installed_packages <- .packages(all.available = TRUE)
    token_packages <- installed_packages[match_with(installed_packages, token)]
    completions <- lapply(token_packages, function(package) {
        list(label = package,
            kind = CompletionItemKind$Module,
            sortText = paste0(sort_prefixes$global, package),
            data = list(type = "package")
        )
    })
    completions
}

#' Complete a function argument
#' @noRd
arg_completion <- function(uri, workspace, point, token, funct, package = NULL, exported_only = TRUE) {
    token_args <- NULL
    token_data <- NULL

    if (is.null(package)) {
        xdoc <- workspace$get_parse_data(uri)$xml_doc
        if (!is.null(xdoc)) {
            row <- point$row + 1
            col <- point$col + 1
            enclosing_scopes <- xdoc_find_enclosing_scopes(xdoc,
                row, col, top = TRUE)
            xpath <- glue(signature_xpath, row = row,
                token_quote = xml_single_quote(funct))
            all_defs <- xml_find_all(enclosing_scopes, xpath)
            if (length(all_defs)) {
                last_def <- all_defs[[length(all_defs)]]
                func_line1 <- as.integer(xml_attr(last_def, "line1"))
                args <- xml_text(xml_find_all(last_def, "SYMBOL_FORMALS"))
                token_args <- args[match_with(args, token)]
                token_data <- list(
                    type = "parameter",
                    funct = funct,
                    uri = uri,
                    line = func_line1
                )
            }
        }

        if (is.null(token_args)) {
            package <- workspace$guess_namespace(funct, isf = TRUE)
        }
    }

    if (!is.null(package)) {
        args <- names(workspace$get_formals(funct, package, exported_only = exported_only))

        if (package == "base" && funct == "options") {
            args <- c(args, names(.Options))
        }

        if (is.character(args)) {
            token_args <- args[match_with(args, token)]
            token_data <- list(
                type = "parameter",
                funct = funct,
                package = package
            )
        }
    }

    completions <- .mapply(function(arg, sort_text) {
        list(label = arg,
            kind = CompletionItemKind$Variable,
            detail = "parameter",
            sortText = sort_text,
            insertText = paste0(arg, " = "),
            insertTextFormat = InsertTextFormat$PlainText,
            data = token_data
        )
    }, list(token_args, sprintf("%s%03d", sort_prefixes$arg, seq_along(token_args))), NULL)

    completions
}


ns_function_completion <- function(ns, token, exported_only, snippet_support) {
    nsname <- ns$package_name
    functs <- ns$get_symbols(want_functs = TRUE, exported_only = exported_only)
    functs <- functs[match_with(functs, token)]
    if (nsname == WORKSPACE) {
        tag <- "[workspace]"
        sort_prefix <- sort_prefixes$workspace
    } else {
        tag <- paste0("{", nsname, "}")
        sort_prefix <- sort_prefixes$global
    }
    if (isTRUE(snippet_support)) {
        completions <- lapply(functs, function(object) {
            list(label = object,
                kind = CompletionItemKind$Function,
                detail = tag,
                sortText = paste0(sort_prefix, object),
                insertText = paste0(object, "($0)"),
                insertTextFormat = InsertTextFormat$Snippet,
                data = list(
                    type = "function",
                    package = nsname
            ))
        })
    } else {
        completions <- lapply(functs, function(object) {
            list(label = object,
                kind = CompletionItemKind$Function,
                detail = tag,
                sortText = paste0(sort_prefix, object),
                data = list(
                    type = "function",
                    package = nsname
            ))
        })
    }
    completions
}

imported_object_completion <- function(workspace, token, snippet_support) {
    completions <- NULL
    for (object in workspace$imported_objects$keys()) {
        if (!match_with(object, token)) {
            next
        }
        nsname <- workspace$imported_objects$get(object)
        ns <- workspace$get_namespace(nsname)
        if (is.null(ns)) {
            next
        }
        if (ns$exists_funct(object)) {
            if (isTRUE(snippet_support)) {
                item <- list(label = object,
                    kind = CompletionItemKind$Function,
                    detail = paste0("{", nsname, "}"),
                    sortText = paste0(sort_prefixes$imported, object),
                    insertText = paste0(object, "($0)"),
                    insertTextFormat = InsertTextFormat$Snippet,
                    data = list(
                        type = "function",
                        package = nsname
                ))
            } else {
                item <- list(label = object,
                    kind = CompletionItemKind$Function,
                    detail = paste0("{", nsname, "}"),
                    sortText = paste0(sort_prefixes$imported, object),
                    data = list(
                        type = "function",
                        package = nsname
                ))
            }
            completions <- append(completions, list(item))
        }
    }
    completions
}


#' Complete any object in the workspace
#' @noRd
workspace_completion <- function(workspace, token,
    package = NULL, exported_only = TRUE, snippet_support = NULL) {
    completions <- list()

    if (is.null(package)) {
        packages <- c(WORKSPACE, workspace$loaded_packages)
    } else {
        packages <- c(package)
    }

    if (is.null(package) || exported_only) {
        for (nsname in packages) {
            ns <- workspace$get_namespace(nsname)
            if (is.null(ns)) {
                next
            }
            if (nsname == WORKSPACE) {
                tag <- "[workspace]"
                sort_prefix <- sort_prefixes$workspace
            } else {
                tag <- paste0("{", nsname, "}")
                sort_prefix <- sort_prefixes$global
            }

            functs_completions <- ns_function_completion(ns, token,
                exported_only = TRUE, snippet_support = snippet_support)

            nonfuncts <- ns$get_symbols(want_functs = FALSE, exported_only = TRUE)
            nonfuncts <- nonfuncts[match_with(nonfuncts, token)]
            nonfuncts_completions <- lapply(nonfuncts, function(object) {
                list(label = object,
                     kind = CompletionItemKind$Field,
                     detail = tag,
                     sortText = paste0(sort_prefix, object),
                     data = list(
                         type = "nonfunction",
                         package = nsname
                     ))
            })
            lazydata <- ns$get_lazydata()
            lazydata <- lazydata[match_with(lazydata, token)]
            lazydata_completions <- lapply(lazydata, function(object) {
                list(label = object,
                     kind = CompletionItemKind$Field,
                     detail = tag,
                     sortText = paste0(sort_prefix, object),
                     data = list(
                         type = "lazydata",
                         package = nsname
                     ))
            })
            completions <- c(completions,
                functs_completions,
                nonfuncts_completions,
                lazydata_completions)
        }
    } else {
        ns <- workspace$get_namespace(package)
        if (!is.null(ns)) {
            tag <- paste0("{", package, "}")
            functs_completions <- ns_function_completion(ns, token,
                exported_only = FALSE, snippet_support = snippet_support)

            nonfuncts <- ns$get_symbols(want_functs = FALSE, exported_only = FALSE)
            nonfuncts <- nonfuncts[match_with(nonfuncts, token)]
            nonfuncts_completions <- lapply(nonfuncts, function(object) {
                list(label = object,
                     kind = CompletionItemKind$Field,
                     detail = tag,
                     sortText = paste0(sort_prefixes$global, object),
                     data = list(
                         type = "nonfunction",
                         package = package
                     ))
            })
            completions <- c(completions,
                functs_completions,
                nonfuncts_completions)
        }
    }

    if (is.null(package)) {
        imported_completions <- imported_object_completion(workspace, token, snippet_support)
        completions <- c(completions, imported_completions)
    }

    completions
}

scope_completion_symbols_xpath <- paste(
    "(*|descendant-or-self::exprlist/*)[self::FUNCTION or self::OP-LAMBDA]/following-sibling::SYMBOL_FORMALS",
    "(*|descendant-or-self::exprlist/*)/LEFT_ASSIGN[not(following-sibling::expr/*[self::FUNCTION or self::OP-LAMBDA])]/preceding-sibling::expr[count(*)=1]/SYMBOL",
    "(*|descendant-or-self::exprlist/*)/RIGHT_ASSIGN[not(preceding-sibling::expr/*[self::FUNCTION or self::OP-LAMBDA])]/following-sibling::expr[count(*)=1]/SYMBOL",
    "(*|descendant-or-self::exprlist/*)/EQ_ASSIGN[not(following-sibling::expr/*[self::FUNCTION or self::OP-LAMBDA])]/preceding-sibling::expr[count(*)=1]/SYMBOL",
    "forcond/SYMBOL",
    sep = "|")

scope_completion_functs_xpath <- paste(
    "(*|descendant-or-self::exprlist/*)/LEFT_ASSIGN[following-sibling::expr/*[self::FUNCTION or self::OP-LAMBDA]]/preceding-sibling::expr[count(*)=1]/SYMBOL",
    "(*|descendant-or-self::exprlist/*)/RIGHT_ASSIGN[preceding-sibling::expr/*[self::FUNCTION or self::OP-LAMBDA]]/following-sibling::expr[count(*)=1]/SYMBOL",
    "(*|descendant-or-self::exprlist/*)/EQ_ASSIGN[following-sibling::expr/*[self::FUNCTION or self::OP-LAMBDA]]/preceding-sibling::expr[count(*)=1]/SYMBOL",
    sep = "|")

scope_completion <- function(uri, workspace, token, point, snippet_support = NULL) {
    xdoc <- workspace$get_parse_data(uri)$xml_doc
    if (is.null(xdoc)) {
        return(list())
    }

    enclosing_scopes <- xdoc_find_enclosing_scopes(xdoc,
        point$row + 1, point$col + 1)

    scope_symbol_nodes <- xml_find_all(enclosing_scopes, scope_completion_symbols_xpath)
    scope_symbol_names <- xml_text(scope_symbol_nodes)
    scope_symbol_lines <- as.integer(xml_attr(scope_symbol_nodes, "line1"))
    scope_symbol_selector <- match_with(scope_symbol_names, token)

    scope_symbol_names <- rev(scope_symbol_names[scope_symbol_selector])
    scope_symbol_lines <- rev(scope_symbol_lines[scope_symbol_selector])
    scope_symbol_selector <- !duplicated(scope_symbol_names)

    scope_symbol_names <- scope_symbol_names[scope_symbol_selector]
    scope_symbol_lines <- scope_symbol_lines[scope_symbol_selector]

    scope_symbol_completions <- .mapply(function(symbol, line) {
        list(
            label = symbol,
            kind = CompletionItemKind$Field,
            sortText = paste0(sort_prefixes$scope, symbol),
            detail = "[scope]",
            data = list(
                type = "nonfunction",
                uri = uri,
                line = line
            )
        )
    }, list(scope_symbol_names, scope_symbol_lines), NULL)

    scope_funct_nodes <- xml_find_all(enclosing_scopes, scope_completion_functs_xpath)
    scope_funct_names <- xml_text(scope_funct_nodes)
    scope_funct_lines <- as.integer(xml_attr(scope_funct_nodes, "line1"))
    scope_funct_selector <- match_with(scope_funct_names, token)

    scope_funct_names <- rev(scope_funct_names[scope_funct_selector])
    scope_funct_lines <- rev(scope_funct_lines[scope_funct_selector])
    scope_funct_selector <- !duplicated(scope_funct_names)

    scope_funct_names <- scope_funct_names[scope_funct_selector]
    scope_funct_lines <- scope_funct_lines[scope_funct_selector]

    if (isTRUE(snippet_support)) {
        scope_funct_completions <- .mapply(function(symbol, line) {
            list(
                label = symbol,
                kind = CompletionItemKind$Function,
                detail = "[scope]",
                sortText = paste0(sort_prefixes$scope, symbol),
                insertText = paste0(symbol, "($0)"),
                insertTextFormat = InsertTextFormat$Snippet,
                data = list(
                    type = "function",
                    uri = uri,
                    line = line
                )
            )
        }, list(scope_funct_names, scope_funct_lines), NULL)
    } else {
        scope_funct_completions <- .mapply(function(symbol, line) {
            list(
                label = symbol,
                kind = CompletionItemKind$Function,
                sortText = paste0(sort_prefixes$scope, symbol),
                detail = "[scope]",
                data = list(
                    type = "function",
                    uri = uri,
                    line = line
                )
            )
        }, list(scope_funct_names, scope_funct_lines), NULL)
    }

    completions <- c(scope_symbol_completions, scope_funct_completions)
    completions
}

token_completion <- function(uri, workspace, token, exclude = NULL) {
    xdoc <- workspace$get_parse_data(uri)$xml_doc
    if (is.null(xdoc)) {
        return(list())
    }

    token_quote <- xml_single_quote(token)

    symbols <- xml_text(xml_find_all(xdoc,
        glue("//*[
            (self::SYMBOL[preceding-sibling::OP-DOLLAR] or self::SYMBOL_SUB) and
            starts-with(text(),'{token_quote}')]",
            token_quote = token_quote
        )
    ))

    if (nzchar(token)) {
        symbols <- c(symbols, xml_text(xml_find_all(xdoc,
            glue("//*[(self::SYMBOL or self::SYMBOL_SUB or self::SYMBOL_FORMALS or self::SYMBOL_FUNCTION_CALL) and
                starts-with(text(),'{token_quote}')]",
                token_quote = token_quote
            )
        )))
    }

    symbols <- setdiff(symbols, exclude)
    token_completions <- lapply(symbols, function(symbol) {
        list(
            label = symbol,
            kind = CompletionItemKind$Text,
            sortText = paste0(sort_prefixes$token, symbol)
        )
    })
}

#' The response to a textDocument/completion request
#' @noRd
completion_reply <- function(id, uri, workspace, document, point, capabilities) {
    if (!check_scope(uri, document, point)) {
        return(Response$new(
            id,
            result = list(
                isIncomplete = FALSE,
                items = list()
            )))
    }

    t0 <- Sys.time()
    snippet_support <- isTRUE(capabilities$completionItem$snippetSupport) &&
        lsp_settings$get("snippet_support")

    token_result <- document$detect_token(point, forward = FALSE)

    full_token <- token_result$full_token
    token <- token_result$token
    package <- token_result$package

    completions <- list()

    if (nzchar(full_token)) {
        if (is.null(package)) {
            completions <- c(
                completions,
                constant_completion(token),
                package_completion(token),
                scope_completion(uri, workspace, token, point, snippet_support))
        }
        completions <- c(
            completions,
            workspace_completion(
                workspace, token, package, token_result$accessor == "::", snippet_support))
    }

    if (token_result$accessor == "") {
        call_result <- document$detect_call(point)
        if (nzchar(call_result$token)) {
            completions <- c(
                completions,
                arg_completion(uri, workspace, point, token,
                    call_result$token, call_result$package,
                    exported_only = call_result$accessor != ":::"))
        }
    }

    if (is.null(token_result$package)) {
        existing_symbols <- vapply(completions, "[[", character(1), "label")
        completions <- c(
            completions,
            token_completion(uri, workspace, token, existing_symbols)
        )
    }

    init_count <- length(completions)
    nmax <- lsp_settings$get("max_completions")

    if (init_count > nmax) {
        isIncomplete <- TRUE
        label_text <- vapply(completions, "[[", character(1), "label")
        sort_text <- vapply(completions, "[[", character(1), "sortText")
        order <- order(!startsWith(label_text, token), sort_text)
        completions <- completions[order][seq_len(nmax)]
    } else {
        isIncomplete <- FALSE
    }

    t1 <- Sys.time()

    logger$info("completions: ", list(
        init_count = init_count,
        final_count = length(completions),
        time = as.numeric(t1 - t0),
        isIncomplete = isIncomplete
    ))

    Response$new(
        id,
        result = list(
            isIncomplete = isIncomplete,
            items = completions
        )
    )
}

#' The response to a completionItem/resolve request
#' @noRd
completion_item_resolve_reply <- function(id, workspace, params, capabilities) {
    resolved <- FALSE
    if (is.null(params$data) || is.null(params$data$type)) {
    } else {
        if (params$data$type == "package") {
            if (length(find.package(params$label, quiet = TRUE))) {
                desc <- utils::packageDescription(params$label, fields = c("Title", "Description"))
                description <- gsub("\\s*\n\\s*", " ", desc$Description)
                params$documentation <- list(
                    kind = "markdown",
                    value = sprintf("**%s**\n\n%s", desc$Title, description)
                )
                resolved <- TRUE
            }
        } else if (params$data$type == "parameter") {
            doc <- NULL
            doc_string <- NULL
            if (is.null(params$data$uri)) {
                doc <- workspace$get_documentation(params$data$funct, params$data$package, isf = TRUE)
            } else {
                document <- workspace$documents$get(params$data$uri)
                func_line1 <- params$data$line
                doc_line1 <- detect_comments(document$content, func_line1 - 1) + 1
                if (doc_line1 < func_line1) {
                    comment <- document$content[doc_line1:(func_line1 - 1)]
                    doc <- convert_comment_to_documentation(comment)
                }
            }
            if (is.list(doc)) {
                doc_string <- doc$arguments[[params$label]]
                if (!is.null(doc_string)) {
                    params$documentation <- list(kind = "markdown", value = doc_string)
                    resolved <- TRUE
                }
            }
        } else if (params$data$type %in% c("constant", "function", "nonfunction", "lazydata")) {
            if (isTRUE(capabilities$completionItem$labelDetailsSupport)) {
                if (params$data$type == "function") {
                    sig <- workspace$get_signature(params$label, params$data$package)
                    if (!is.null(sig)) {
                        params$labelDetails <- list(
                            detail = substr(sig, nchar(params$label) + 1, nchar(sig))
                        )
                    }
                }
            }

            doc <- NULL
            doc_string <- NULL
            if (is.null(params$data$uri)) {
                doc <- workspace$get_documentation(params$label, params$data$package,
                    isf = params$data$type == "function")
            } else {
                document <- workspace$documents$get(params$data$uri)
                token_line1 <- params$data$line
                doc_line1 <- detect_comments(document$content, token_line1 - 1) + 1
                if (doc_line1 < token_line1) {
                    comment <- document$content[doc_line1:(token_line1 - 1)]
                    doc <- convert_comment_to_documentation(comment)
                }
            }

            if (is.character(doc)) {
                doc_string <- doc
            } else if (is.list(doc)) {
                doc_string <- doc$description
            }

            if (!is.null(doc_string)) {
                params$documentation <- list(kind = "markdown", value = doc_string)
                resolved <- TRUE
            }
        }
    }

    params$data <- NULL
    Response$new(
        id,
        result = params
    )
}
