#parsepost.py

def parse_post( txt ):
  lines = txt.split( "\n" )
  acc = ""
  for line in lines:
    if line.strip().startswith( ">" ):
      line = "<div class=\"greentext\">" + line + "</div>"
    acc += line + "\n"
  return acc
