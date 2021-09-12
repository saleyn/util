#==============================================================================
# Convert README.md GitHub document to overview.edoc
#
# Copyright (c) 2021 Serge Aleynikov
#==============================================================================
BEGIN {
    in_code3=0
    in_code2=0
    in_code1=0
    in_code=0
    in_list_unordered=0
    in_list_ordered=0
    in_paragraph=0
}

!in_code {
  # Emails
  if (match($0, /<([^@.>" ]+\(at\)[^.>" ]+\.[^>" ]+)>/)) {
    $0 = gensub(/<([^@.>" ]+\(at\)[^.>" ]+\.[^>" ]+)>/,
                "\\&lt;\\1\\&gt;", "g")
  }
  # Emails
  else if (match($0, /<([^@.>" ]+@[^.>" ]+\.[^>" ]+)>/)) {
    $0 = gensub(/<([^@.>" ]+@[^.>" ]+\.[^>" ]+)>/,
                "\\&lt;<a href=\"mailto:\\1\">\\1</a>\\&gt;", "g")
  }
  # URLs
  else if (match($0, /\[[^\[]*\[[^\]]*\] *\(.+\.svg[^\)]*\)\] *\( *([^\)]+)\)/)) {
    $0 = gensub(/\[[^\[]*\[[^\]]+\] *\(([^)]+)\)\] *\( *([^\)]+) *\)/, "<a href=\"\\2\"><img src=\"\\1\"/></a>", "g")
  }
  else if (match($0, /\[.+\] *\( *(http|ftp).+\)/)) {
    #$0 = gensub(/\[[^\[]*?\[[^\]]+\]\(?([^)]+)\)?\]\( *([^\)]+)\)/, "<a href=\"\\2\">\\1</a>", "g")
    #$0 = gensub(/\[\\!\[[^\]]+\]\(?([^)]+)\)?\]\( *([^\)]+)\)/, "<a href=\"\\2\">\\1</a>", "g")
    $0 = gensub(/\[([^\]]+)\]\( *([^\)]+)\)/, "<a href=\"\\2\">\\1</a>", "g")
  }
  else {
    tmp = $0
    b = ""
    while (match(tmp, /(https?|s?ftp):\/\/[a-zA-Z0-9\-\.]+\.[a-zA-Z]{2,3}(\/\S*)?/, res)) {
      b = b substr(tmp, 1, RSTART-1) "[" res[0] "]"
      tmp = substr(tmp, RSTART+RLENGTH)
    }
    if (length(b) > 0) $0 = b
    delete res
  }
}

{
  # copyright
  gsub(/Copyright +\([cC]\)/, "Copyright @copy;")
  #gsub(/\([Cc]\)/, "@copy;")
  gsub(/\([rR]\)/, "\\&reg;")
  gsub(/\(tm\)/,"\\&trade;")
  gsub(/\(TM\)/,"\\&trade;")
  # Match ** (\52 = '*')

  # The author is printed in front of the @doc
  if (match($0, /\*\*[Aa]uthors?\*\*.*$/))
    next

  $0 = gensub(/\*\*([^\*]+)\*\*/, "<strong>\\1</strong>", "g")
}

# Print main title
/^#[^#]/ && !main_title && !in_code {
  title = gensub(/^# */, "", "g")
  print "@version", vsn
  if (author)
    print "@author", author
  print "@doc\n"
  print "==",title,"==\n"
  main_title=1
  next
}

# Display code blocks
/```/ {
  par = paragraph
  pop_all_lists()
  if (!par && !in_code3)
    print "\n"

  gsub(/```[^ ]*/, "```")
  in_code3 = !in_code3
  in_code = in_code1 || in_code2 || in_code3
  if (!in_code3)
    sub(/```/, "'''")

  print_paragraph()
  if (!par && !in_code3)
    print "\n"

  print
  next
}

# Display code words
/``/ && !in_code {
  in_code2 = replace_code("``", "''", in_code2)
  in_code  = in_code1 || in_code2 || in_code3
}
    
# Display code words
/`[^`']/ && !in_code {
  in_code1 = replace_code("`", "'", in_code1)
  in_code  = in_code1 || in_code2 || in_code3
}

# close unordered list
in_list_unordered {
  if (match($0, /^ *[-*] /) && RSTART+RLENGTH == li_offset) {
    list_li_close()
  } else if ((match($0, /^ *[-*] /) || match($0, /^\s+/) || match($0, /^[^\s]/)) && RSTART+RLENGTH < li_offset) {
    list_pop()
  }
}

# close ordered list
in_list_ordered {
  if (match($0,/^ *[0-9]+\. /) && RSTART+RLENGTH == li_offset) {
    list_li_close()
  } else if ((match($0,/^ *[0-9]+\. /) || match($0, /^ +/) || match($0, /^[^\s]/)) && RSTART+RLENGTH < li_offset) {
    list_pop()
  }
}

# Print titles
/^#/ && !in_code {
  match($0, /^#+/)
  n = RLENGTH-1
  if (n < 1) n = 1

  gsub(/^#+/, "")
  gsub(/#+$/, "")

  $0 = sprintf("<h%d>%s</h%d>", n, $0, n)

  pop_all_lists()
}

# Display tables
!/^\|/ && !in_code && in_table {
  print_paragraph()
  print "</table>"
  in_table = 0
  lastrow  = ""
}
/\|[^\|]+\|/ && !in_code {
  if (!in_table) {
    print_paragraph()
    print("<table class=\"tab\">\n")
    in_table=1
    lastrow = $0
    next
  } else if (in_table++ == 1 && match($0, /\| *-+ *\|/)) {
    print_table_row("h", lastrow)
    lastrow = ""
    next
  }
  print_table_row("d", $0)
  next
}

{
  n = -1

  # display unordered lists
  if(/^ *[-\*] /) {
    match($0,/[-\*] /)
    update_and_maybe_push_list("ul")
  # display ordered lists
  } else if(/^ *[0-9]+\. /) {
    match($0,/[0-9]+\. /)
    update_and_maybe_push_list("ol")
  }
}

{
  # Close paragraph if curr line is empty
  if(length($0) == 0 && in_paragraph == 1 && in_code == 0) {
    in_paragraph=0
    paragraph++
  }
  # Still in a paragraph
  if(length($0) != 0 && in_paragraph == 1) {
    print_line()
  }
  # Open a paragraph if the prev line is empty
  if(length(prev_line)==0 && in_paragraph==0) {
    in_paragraph=1
    print_line()
  }
  prev_line = $0
}

!in_list_unordered && !in_list_ordered && paragraph {
  print_paragraph()
}


END {
  if      (in_code3) print  "'''"
  else if (in_code2) printf "''"
  else if (in_code1) printf "'"

  if      (in_table) print  "</table>"

  pop_all_lists()
}

function update_and_maybe_push_list(li) {
  n  = RSTART+RLENGTH
  $0 = substr($0, n)
  style = ""
  if (match($0, /^\[.\] /)) {
    style = " type=\"squares\""
    $0    = substr($0, RLENGTH) 
  }
  if (n > li_offset) {
    # New unordered list
    list_push(n, li)
    printf "%*s<%s>\n", n-2, " ", li
  }
  printf "%*s<li>", n, " "
}
function list_push(offset, type) {
  i = list_pos++
  list_offsets[i]   = offset
  list_types[i]     = type
  list_closed[i]    = false
  li_offset         = offset
  in_list_unordered = type == "ul"
  in_list_ordered   = type == "ol"
  return i
}
function list_pop()  {
  offset = list_offset()
  if (list_pos == 0) return offset
  list_li_close()
  printf "%*s</%s>\n", offset-2, " ", list_type()
  list_pos--
  delete list_offsets[list_pos]
  delete list_types[list_pos]
  delete list_closed[list_pos]
  type              = list_type()
  in_list_unordered = type == "ul"
  in_list_ordered   = type == "ol"
  li_offset         = list_offset()
  return offset
}
function pop_all_lists() {
  type = list_type()
  while (type != "") {
    list_pop()
    type = list_type()
  }
  print_paragraph()
}
function print_paragraph() {
  for(; paragraph > 0; --paragraph)
    printf "\n"
}
function print_line() {
  if (in_code3)
    gsub(/@/,  "\\&commat;")
  gsub(/&#124;/, "|")   # Substitute the pipe symbol back to original
  print
}
# Replace code words delimited by open_qq
# (e.g. open_qq = "``" or "`"). close_qq is "''" or "'".
function replace_code(open_qq, close_qq, in_code_var) {
  sub(/ +$/, "")
  gsub(/'/, "\\&apos;")
  delete arr
  n = split($0, arr, open_qq)
  i = 2
  for(; i <= n; ++i) {
    in_code_var = !in_code_var
    if (in_code_var && i < n) {
      # Replace "|" symbol to eliminate confusion for tables
      gsub(/\|/, "\\&#124;", arr[i])
      arr[i] = arr[i] close_qq
    }
    arr[i] = sprintf("%s%s", (in_code_var ? open_qq : ""), arr[i])
  }

  $0 = join(arr, 1, n)
  #gsub(/@@@>>>/, "\\&#x22;") # Replace single quotes
  delete arr
  return in_code_var
}
function print_table_row(type, str) {
  # Print rows
  delim = sprintf("</t%s><t%s class=\"tab\">", type, type)
  gsub(/ *\| */, delim, str)
  sub(/<\/t[dh]>/,     "<tr class=\"tab\">", str)  # Remove first occurance of </td> with <tr>
  sub(/<t[dh][^>]+>$/, "</tr>",              str)  # Replace last occrance of <td> with </tr>
  print str
}
function list_offset()    { return list_pos ? list_offsets[list_pos-1] :  0 }
function list_type()      { return list_pos ? list_types[list_pos-1]   : "" }
function list_li_close()  {
  if (!list_pos) return
  printf("%*s</li>\n", list_offsets[list_pos-1], " ")
  list_closed[list_pos-1] = false
  print_paragraph()
}
# An optional additional argument is the separator to use when joining the strings
# back together. If the caller supplies a nonempty value, join() uses it; if it is not
# supplied, it has a null value. In this case, join() uses a single space as a default
# separator for the strings.
function join(array, start, end, sep, result, i)
{
  if (start == 0)
    start = 1
  if (end == 0)
    for(i in array) end++
  result = array[start]
  for (i = start + 1; i <= end; i++)
    result = result sep array[i]
  return result
}
