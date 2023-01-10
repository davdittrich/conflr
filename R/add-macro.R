# Copyright (C) 2022 davd
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.

#' Creates note
#'
#' @name confl_add_node
#' @param title
#'   The content title to convert.
#' @param body
#'   The content body to convert.
#'
#' @return
#'   The API response as a list.
#' @export
confl_add_note <- function(title = "", body = "") {

 glue::glue('<div class="conflr-note" style="clear:both;width=32em;float:right; margin-left:2em">',
   glue:glue('{{note:title={title}|icon=false}}{body}{{note}}') |> 
   confl_contentbody_convert() |>
   stringi::stri_replace_all_regex('<.?p>',''),
   '</div>') -> x
  
 x
}

#' Creates tip
#'
#' @name confl_add_tip
#' @param title
#'   The content title to convert.
#' @param body
#'   The content body to convert.
#'
#' @return
#'   The API response as a list.
#' @export
#' @export
confl_add_tip <- function(title = "", body = "") {

 glue::glue('<div class="conflr-note" style="clear:both;width=32em;float:right; margin-left:2em">',
   glue:glue('{{tip:title={title}|icon=false}}{body}{{tip}}') |> 
   confl_contentbody_convert() |>
   stringi::stri_replace_all_regex('<.?p>',''),
   '</div>') -> x
  
 x
}
