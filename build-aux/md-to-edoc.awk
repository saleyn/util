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
  else if (match($0, /\[[^\[]*?\]\(.+\.svg\)\]\( *([^\)]+)\)/)) {
    $0 = gensub(/\[[^\[]*?\[[^\]]+\]\(?([^)]+)\)?\]\( *([^\)]+)\)/, "<a href=\"\\2\"><img src=\"\\1\"/></a>", "g")
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
  gsub(/\(c\)/, "\\&#169;")
  gsub(/\(C\)/, "\\&#169;")
  gsub(/\(R\)/, "\\&#174;")
  gsub(/\(r\)/, "\\&#174;")
  gsub(/\(tm\)/,"\\&#153;")
  gsub(/\(TM\)/,"\\&#153;")
  gsub(/@/,     "\\&#174;")
  # Match ** (\52 = '*')
  $0 = gensub(/\*\*([^\*]+)\*\*/, "<strong>\\1</strong>", "g")
}

# Print main title
/^#[^#]/ && !main_title {
  title = gensub(/^# */, "", "g")
  print "@version", vsn
  print "@doc\n"
  print "==",title,"==\n"
  main_title=1
  next
}

# Display code blocks
/```/ {
  pop_all_lists()
  gsub(/```[^ ]*/, "```")
  in_code3 = !in_code3
  in_code = in_code1 || in_code2 || in_code3
  if (!in_code3) sub(/```/, "'''")
  print
  next
}

# Display code words
/``/ && !in_code3 {
  gsub(/'/, "@@@>>>")
  delete arr
  n = split($0, arr, "``")
  i = 2
  for(; i <= n; ++i) {
    in_code2 = !in_code2
    arr[i] = sprintf("%s%s%s", (in_code2 ? "``" : ""), arr[i], (in_code2 && i < n) ? "''" : "")
  }

  in_code  = in_code1 || in_code2 || in_code3
  $0 = join(a, 1, n)
  gsub(/@@@>>>/, "\\&#x22;") # Replace single quotes
  delete arr
}
    
# Display code words
/`[^`']/ && !in_code3 && !in_code2 {
  sub(/ +$/, "")
  gsub(/'/, "@@@>>>")
  delete arr
  n = split($0, arr, "`")
  i = 2
  for(; i <= n; ++i) {
    in_code1 = !in_code1
    arr[i] = sprintf("%s%s%s", (in_code1 ? "`" : ""), arr[i], (in_code1 && i < n) ? "'" : "")
  }

  in_code  = in_code1 || in_code2 || in_code3
  $0 = join(a, 1, n)
  gsub(/@@@>>>/, "\\&#x22;") # Replace single quotes
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
/^#/ {
  gsub(/#/, "=")
  match($0, /^=+/)
  n = RLENGTH < 2 ? 2 : RLENGTH
  if (!match($0, /=$/)) {
    s = sprintf(" %s", substr("====", 1, n))
    gsub(/$/, s)
  }
  pop_all_lists()
}

# Display tables
!in_code && in_table && ! /^\|/ {
  print "</table>"
  in_table = 0
}
!in_code && /\| *-+ *\|/ {
  # Skip separators
  next
}
!in_code && /\|[^\|]+\|/ && !/\| *-+ *\|/ {
  # Print rows
  delim = in_table ? "</td><td class=\"tab\">" : "</th><th class=\"tab\">"
  if (!in_table) {
    print("<table class=\"tab\">\n")
    in_table=1
  }
  gsub(/ *\| */, delim)
  sub(/<\/t[dh]>/,     "<tr class=\"tab\">")  # Remove first occurance of </td> with <tr>
  sub(/<t[dh][^>]+>$/, "</tr>")               # Replace last occrance of <td> with </tr>
  print
  next
}

{
  n = -1

  # display unordered lists
  if(match($0,/^ *[-\*] /)) {
    n  = RSTART+RLENGTH
    $0 = substr($0, n)
    if (n > li_offset) {
      # New unordered list
      list_push(n, "ul")
      printf "%*s<ul>\n", n-2, " "
    }
    printf "%*s<li>", n, " "

  # display ordered lists
  } else if(match($0,/^ *[0-9]+\. /)) {
    match($0, /[0-9]+\. /)
    n  = RSTART+RLENGTH
    $0 = substr($0, n)
    if (n > li_offset) {
      # New ordered list
      list_push(n, "ol")
      printf "%*s<ol>\n", n-2, " "
    }
    printf "%*s<li>", n, " "
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
    print
  }
  # Open a paragraph if the prev line is empty
  if(length(prev_line)==0 && in_paragraph==0) {
    in_paragraph=1
    print
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

  if(in_paragraph)
    printf "\n"
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
}
function print_paragraph() {
  for(; paragraph > 0; --paragraph)
    printf "\n"
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
