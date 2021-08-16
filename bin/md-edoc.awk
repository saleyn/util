#==============================================================================
# Convert *.md GitHub documents to overview.edoc
# Author: Serge Aleynikov
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
                "<li class=\"ref email\">\\&lt;\\1\\&gt;</li>", "g")
  }
  # Emails
  else if (match($0, /<([^@.>" ]+@[^.>" ]+\.[^>" ]+)>/)) {
    $0 = gensub(/<([^@.>" ]+@[^.>" ]+\.[^>" ]+)>/,
                "<li class=\"ref email\">\\&lt;<a href=\"mailto:\\1\">\\1</a>\\&gt;</li>", "g")
  }
  # URLs
  else if (match($0, /\[[^\[]*?\]\(.+\.svg\)\]\( *([^\)]+)\)/)) {
    $0 = gensub(/\[[^\[]*?\[[^\]]+\]\(?([^)]+)\)?\]\( *([^\)]+)\)/, "<a href=\"\\2\"><img src=\"\\1\"/></a>", "g")
  }
  else if (match($0, /\[.+\] *\( *(http|ftp).+\)/)) {
    #$0 = gensub(/\[[^\[]*?\[[^\]]+\]\(?([^)]+)\)?\]\( *([^\)]+)\)/, "<a href=\"\\2\">\\1</a>", "g")
    #$0 = gensub(/\[\\!\[[^\]]+\]\(?([^)]+)\)?\]\( *([^\)]+)\)/, "<a href=\"\\2\">\\1</a>", "g")
    $0 = gensub(/\[([^\]]+)\( *([^\)]+)\)/, "<a href=\"\\2\">\\1</a>", "g")
  }
  else {
    # escape < > characters
    #gsub(/</,"\\&lt;");
    #gsub(/>/,"\\&gt;");
    
    # copyright
    gsub(/\(c\)/, "\\&#169;")
    gsub(/\(C\)/, "\\&#169;")
    gsub(/\(R\)/, "\\&#174;")
    gsub(/\(r\)/, "\\&#174;")
    gsub(/\(tm\)/,"\\&#153;")
    gsub(/\(TM\)/,"\\&#153;")
  }
}

# Print main title
/^#[^#]/ && !main_title {
  title = gensub(/^# */, "", "g")
  #out   = sprintf("@title %s\n@doc %s\n@version %s\n", title, title, vsn)
  #gsub(/^# */, out)
  print "  ",title, "\n"
  print "@version", vsn
  print "@doc",title,"\n"
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
  delete a
  gsub(/'/, "@@@>>>")
  n = split($0, a, "``")
  i = 2
  for(; i <= n; ++i) {
    in_code2 = !in_code2
    a[i] = sprintf("%s%s%s", (in_code2 ? "``" : ""), a[i], (in_code2 && i < n) ? "''" : "")
  }

  in_code  = in_code1 || in_code2 || in_code3
  $0 = join(a, 1, n)
  gsub(/@@@>>>/, "\\&#x22;") # Replace single quotes
}
    
# Display code words
/`[^`']/ && !in_code3 && !in_code2 {
  delete a
  sub(/ +$/, "")
  gsub(/'/, "@@@>>>")
  n = split($0, a, "`")
  i = 2

  for(; i <= n; ++i) {
    in_code1 = !in_code1
    a[i] = sprintf("%s%s%s", (in_code1 ? "`" : ""), a[i], (in_code1 && i < n) ? "'" : "")
  }

  in_code  = in_code1 || in_code2 || in_code3
  $0 = join(a, 1, n)
  gsub(/@@@>>>/, "\\&#x22;") # Replace single quotes
}

# close unordered list
in_list_unordered {
  if (match($0, /^ *[-*] /) && RSTART+RLENGTH == li_offset) {
    print_saved()
    printf "%*s</li>\n", li_offset, " "
  #} else if ((match($0, /^ *[-*] /) || match($0, /^\s+/) || match($0, /^$/)) && RSTART+RLENGTH < li_offset) {
  } else if ((match($0, /^ *[-*] /) || match($0, /^\s+/) || match($0, /^[^\s]/)) && RSTART+RLENGTH < li_offset) {
    print_saved()
    printf "%*s</li>\n", li_offset, " "
    old_offset = list_pop()
  }
}

# close ordered list
in_list_ordered {
  if (match($0,/^ *[0-9]+\. /) && RSTART+RLENGTH == li_offset) {
    print_saved()
    printf "%*s</li>\n", li_offset, " "
  #} else if ((match($0,/^ *[0-9]+\. /) || match($0, /^ +/) || match($0, /^$/)) && RSTART+RLENGTH < li_offset) {
  } else if ((match($0,/^ *[0-9]+\. /) || match($0, /^ +/) || match($0, /^[^\s]/)) && RSTART+RLENGTH < li_offset) {
    print_saved()
    printf "%*s</li>\n", li_offset, " "
    old_offset = list_pop()
  }
}

# Print titles
/^#/ {
  gsub(/#/, "=")
  match($0, /^=+/)
  n = RLENGTH
  if (!match($0, /=$/)) {
    s = sprintf(" %s", substr("===", 1, n))
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
  print_saved()
  next
}
!in_code && /\|[^\|]+\|/ && !/\| *-+ *\|/ {
  # Print rows
  print_saved()
  delim = in_table ? "</td><td>" : "</th><th>"
  if (!in_table) {
    print("<table>\n")
    in_table=1
  }
  gsub(/ *\| */, delim)
  sub(/<\/t[dh]>/, "<tr>") # Remove first occurance of </td> with <tr>
  sub(/<t[dh]>$/, "</tr>") # Replace last occrance of <td> with </tr>
  print
  next
}

{
  # display unordered lists
  if(match($0,/^ *[-\*] /)) {
    n  = RSTART+RLENGTH
    $0 = substr($0, n)
    print_saved()
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
    print_saved()
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
    printf "\n"
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

END {
  if      (in_code3) print_line("'''")
  else if (in_code2) print_line("''")
  else if (in_code1) print_line("'")

  if      (in_table) print_line("</table>")

  pop_all_lists()

  if(in_paragraph)
    printf "\n"    # "</p>"
}

function list_push(offset, type) {
  i = list_pos++
  list_offsets[i]   = offset
  list_types[i]     = type
  li_offset         = offset
  in_list_unordered = type == "ul"
  in_list_ordered   = type == "ol"
  return i
}
function list_pop()  {
  offset = list_offset()
  if (list_pos == 0) return offset
  printf "%*s</%s>\n", offset-2, " ", list_type()
  list_pos--
  delete list_offsets[list_pos]
  delete list_types[list_pos]
  type              = list_type()
  in_list_unordered = type == "ul"
  in_list_unordered = type == "ol"
  li_offset         = list_offset()
  return offset
}
function pop_all_lists() {
  print_saved()
  type = list_type()
  while (type != "") {
    list_pop()
    type = list_type()
  }
}

function list_offset()    { return list_pos ? list_offsets[list_pos-1] :  0 }
function list_type()      { return list_pos ? list_types[list_pos-1]   : "" }
function print_line(line) {
  #saved_lines[n_saved++] = line
  printf "%s", line
}
function print_saved()    {
  #for (i=1; i <= n_saved; ++i) printf(saved_lines[i]); delete saved_lines; n_saved=0;
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
