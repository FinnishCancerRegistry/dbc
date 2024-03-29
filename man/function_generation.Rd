% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function_generation.R
\name{function_generation}
\alias{function_generation}
\alias{generate_function_from_expressions}
\alias{generate_report_function_wrapper}
\alias{generate_script_from_expressions}
\alias{generate_report_function_wrapper_script}
\title{Function Generation}
\usage{
generate_function_from_expressions(
  fun_nm,
  fun_type,
  expressions,
  assertion_type = NULL,
  fail_messages = NULL,
  pass_messages = NULL,
  extra_args = NULL
)

generate_report_function_wrapper(
  report_fun_nm,
  fun_nm,
  fun_type,
  assertion_type = NULL,
  extra_args = NULL
)

generate_script_from_expressions(
  tgt_script_path,
  df,
  fun_type,
  assertion_type = NULL
)

generate_report_function_wrapper_script(
  tgt_script_path,
  report_fun_nms,
  extra_arg_sets,
  fun_type,
  assertion_type = NULL
)
}
\arguments{
\item{fun_nm}{\verb{[character]} (no default)

Name of function to generate.}

\item{fun_type}{\verb{[character]} (no default)

One of \code{c("test", "report", "assertion")}.}

\item{expressions}{\verb{[character]} (no default)

One or more R expressions as \code{character} strings. These will be evaluted
in the given order in the generated function.}

\item{assertion_type}{\verb{[NULL, character]} (default \code{NULL})

If \code{fun_type == "assertion"}, this must be one of the alternatives given
by \verb{[assertion_types]}.}

\item{fail_messages}{\verb{[NULL, character]} (default \code{NULL})

If \code{fun_type != "test"}, this cannot be \code{NULL} but must give a failure
message for each \code{expressions} element --- though \code{NA_character_} is allowed.
\code{NA_character_} will be replaced internally by a generic message.}

\item{pass_messages}{\verb{[NULL, character]} (default \code{NULL})

If \code{fun_type == "report"}, see \verb{[expressions_to_report]}. Else ignored.}

\item{extra_args}{\verb{[NULL, character]} (default \code{NULL})
\itemize{
\item \code{NULL}: The function will take no extra arguments.
\item \code{character}: The function will take extra arguments corresponding to the
names of this object, and their defaults will be the elements of the
object. E.g. \code{c(my_extra_arg = "NULL")}.
}}

\item{report_fun_nm}{\verb{[character]} (no default)

Name of function to generate wrapper for --- e.g. \code{"dbc::report_is_integer"}.}

\item{tgt_script_path}{\verb{[character]} (no default)

Path to script to be generated. Will be overwritten if it exists.}

\item{df}{\verb{[data.frame]} (no default)

\code{data.frame} of metadata on functions to generate. Columns:
\itemize{
\item \code{fun_nm_suffix}: These will be the suffixes of generated function
names. E.g. \code{"has_length_one"}.
\item \code{expression_set}: A list column, where each element is a vector of
character strings of R expressions. These will be the actual tests
performed. See
\code{expressions} of \code{dbc::generate_function_from_expressions}.
\item \code{fail_message_set}: This list column contains one \code{fail_message}
per corresponding expression in \code{expression_set}. See
\code{fail_messages} of \code{dbc::generate_function_from_expressions}.
\item \code{pass_message_set}: This list column contains one \code{pass_message}
per corresponding expression in \code{expression_set}. See
\code{pass_messages} of \code{dbc::generate_function_from_expressions}.
\item \code{extra_arg_set}: This list column contains a character string vector
or \code{NULL} for every function. See arg \code{extra_args} of
\code{dbc::generate_function_from_expressions}.
}}

\item{report_fun_nms}{\verb{[character]} (no default)

One or more report function names. Each in turn passed to
\code{dbc::generate_report_function_wrapper} arg \code{report_fun_nm}.}

\item{extra_arg_sets}{\verb{[list]} (no default)

A set of of \code{extra_args} for each element of \code{report_fun_nms} ---
each \code{NULL} or a character string vector.}
}
\description{
Generate report, test, and assertion functions.
}
