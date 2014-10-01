open Types

let compress = function
  | Ident "aliceblue"               -> Hexcolor "f0f8ff"
  | Ident "antiquewhite"            -> Hexcolor "faebd7"
  (*| Ident "aqua"                    -> Hexcolor "0ff"*)
  | Ident "aquamarine"              -> Hexcolor "7fffd4"
  | Hexcolor "f0ffff"               -> Ident "azure"
  | Hexcolor "f5f5dc"               -> Ident "beige"
  | Hexcolor "ffe4c4"               -> Ident "bisque"
  | Ident "black"                   -> Hexcolor "000"
  | Ident "blanchedalmond"          -> Hexcolor "ffebcd"
  (*| Ident "blue"                    -> Hexcolor "00f"*)
  | Ident "blueviolet"              -> Hexcolor "8a2be2"
  | Hexcolor "a52a2a"               -> Ident "brown"
  | Ident "burlywood"               -> Hexcolor "deb887"
  | Ident "cadetblue"               -> Hexcolor "5f9ea0"
  | Ident "chartreuse"              -> Hexcolor "7fff00"
  | Ident "chocolate"               -> Hexcolor "d2691e"
  | Hexcolor "ff7f50"               -> Ident "coral"
  | Ident "cornflowerblue"          -> Hexcolor "6495ed"
  | Ident "cornsilk"                -> Hexcolor "fff8dc"
  (*| Ident "cyan"                    -> Hexcolor "0ff"*)
  | Ident "darkblue"                -> Hexcolor "00008b"
  | Ident "darkcyan"                -> Hexcolor "008b8b"
  | Ident "darkgoldenrod"           -> Hexcolor "b8860b"
  | Ident "darkgray"                -> Hexcolor "a9a9a9"
  | Ident "darkgreen"               -> Hexcolor "006400"
  | Ident "darkkhaki"               -> Hexcolor "bdb76b"
  | Ident "darkmagenta"             -> Hexcolor "8b008b"
  | Ident "darkolivegreen"          -> Hexcolor "556b2f"
  | Ident "darkorange"              -> Hexcolor "ff8c00"
  | Ident "darkorchid"              -> Hexcolor "9932cc"
  | Ident "darksalmon"              -> Hexcolor "e9967a"
  | Ident "darkseagreen"            -> Hexcolor "8fbc8f"
  | Ident "darkslateblue"           -> Hexcolor "483d8b"
  | Ident "darkslategray"           -> Hexcolor "2f4f4f"
  | Ident "darkturquoise"           -> Hexcolor "00ced1"
  | Ident "darkviolet"              -> Hexcolor "9400d3"
  | Ident "deeppink"                -> Hexcolor "ff1493"
  | Ident "deepskyblue"             -> Hexcolor "00bfff"
  | Ident "dodgerblue"              -> Hexcolor "1e90ff"
  | Ident "feldspar"                -> Hexcolor "d19275"
  | Ident "firebrick"               -> Hexcolor "b22222"
  | Ident "floralwhite"             -> Hexcolor "fffaf0"
  | Ident "forestgreen"             -> Hexcolor "228b22"
  | Ident "fuchsia"                 -> Hexcolor "f0f"
  | Ident "gainsboro"               -> Hexcolor "dcdcdc"
  | Ident "ghostwhite"              -> Hexcolor "f8f8ff"
  | Ident "goldenrod"               -> Hexcolor "daa520"
  | Ident "greenyellow"             -> Hexcolor "adff2f"
  | Ident "honeydew"                -> Hexcolor "f0fff0"
  | Ident "indianred"               -> Hexcolor "cd5c5c"
  | Hexcolor "4b0082"               -> Ident "indigo"
  | Hexcolor "ffd700"               -> Ident "gold"
  | Hexcolor "808080"               -> Ident "gray"
  | Hexcolor "008000"               -> Ident "green"
  | Hexcolor "fffff0"               -> Ident "ivory"
  | Hexcolor "f0e68c"               -> Ident "khaki"
  | Ident "lavender"                -> Hexcolor "e6e6fa"
  | Ident "lavenderblush"           -> Hexcolor "fff0f5"
  | Ident "lawngreen"               -> Hexcolor "7cfc00"
  | Ident "lemonchiffon"            -> Hexcolor "fffacd"
  | Ident "lightblue"               -> Hexcolor "add8e6"
  | Ident "lightcoral"              -> Hexcolor "f08080"
  | Ident "lightcyan"               -> Hexcolor "e0ffff"
  | Ident "lightgoldenrodyellow"    -> Hexcolor "fafad2"
  | Ident "lightgrey"               -> Hexcolor "d3d3d3"
  | Ident "lightgreen"              -> Hexcolor "90ee90"
  | Ident "lightpink"               -> Hexcolor "ffb6c1"
  | Ident "lightsalmon"             -> Hexcolor "ffa07a"
  | Ident "lightseagreen"           -> Hexcolor "20b2aa"
  | Ident "lightskyblue"            -> Hexcolor "87cefa"
  | Ident "lightslateblue"          -> Hexcolor "8470ff"
  | Ident "lightslategray"          -> Hexcolor "789"
  | Ident "lightsteelblue"          -> Hexcolor "b0c4de"
  | Ident "lightyellow"             -> Hexcolor "ffffe0"
  | Ident "limegreen"               -> Hexcolor "32cd32"
  | Hexcolor "faf0e6"               -> Ident "linen"
  | Ident "magenta"                 -> Hexcolor "f0f"
  | Hexcolor "800000"               -> Ident "maroon"
  | Ident "mediumaquamarine"        -> Hexcolor "66cdaa"
  | Ident "mediumblue"              -> Hexcolor "0000cd"
  | Ident "mediumorchid"            -> Hexcolor "ba55d3"
  | Ident "mediumpurple"            -> Hexcolor "9370d8"
  | Ident "mediumseagreen"          -> Hexcolor "3cb371"
  | Ident "mediumslateblue"         -> Hexcolor "7b68ee"
  | Ident "mediumspringgreen"       -> Hexcolor "00fa9a"
  | Ident "mediumturquoise"         -> Hexcolor "48d1cc"
  | Ident "mediumvioletred"         -> Hexcolor "c71585"
  | Ident "midnightblue"            -> Hexcolor "191970"
  | Ident "mintcream"               -> Hexcolor "f5fffa"
  | Ident "mistyrose"               -> Hexcolor "ffe4e1"
  | Ident "moccasin"                -> Hexcolor "ffe4b5"
  | Ident "navajowhite"             -> Hexcolor "ffdead"
  | Hexcolor "000080"               -> Ident "navy"
  | Hexcolor "808000"               -> Ident "olive"
  | Ident "olivedrab"               -> Hexcolor "6b8e23"
  | Hexcolor "ffa500"               -> Ident "orange"
  | Ident "orangered"               -> Hexcolor "ff4500"
  | Hexcolor "da70d6"               -> Ident "orchid"
  | Ident "palegoldenrod"           -> Hexcolor "eee8aa"
  | Ident "palegreen"               -> Hexcolor "98fb98"
  | Ident "paleturquoise"           -> Hexcolor "afeeee"
  | Ident "palevioletred"           -> Hexcolor "d87093"
  | Ident "papayawhip"              -> Hexcolor "ffefd5"
  | Ident "peachpuff"               -> Hexcolor "ffdab9"
  | Hexcolor "cd853f"               -> Ident "peru"
  | Hexcolor "ffc0cb"               -> Ident "pink"
  | Hexcolor "dda0dd"               -> Ident "plum"
  | Ident "powderblue"              -> Hexcolor "b0e0e6"
  | Hexcolor "800080"               -> Ident "purple"
  | Ident "red"                     -> Hexcolor "f00"
  | Ident "rosybrown"               -> Hexcolor "bc8f8f"
  | Ident "royalblue"               -> Hexcolor "4169e1"
  | Ident "saddlebrown"             -> Hexcolor "8b4513"
  | Hexcolor "fa8072"               -> Ident "salmon"
  | Ident "sandybrown"              -> Hexcolor "f4a460"
  | Ident "seagreen"                -> Hexcolor "2e8b57"
  | Ident "seashell"                -> Hexcolor "fff5ee"
  | Hexcolor "a0522d"               -> Ident "sienna"
  | Hexcolor "c0c0c0"               -> Ident "silver"
  | Ident "slateblue"               -> Hexcolor "6a5acd"
  | Ident "slategray"               -> Hexcolor "708090"
  | Hexcolor "fffafa"               -> Ident "snow"
  | Ident "springgreen"             -> Hexcolor "00ff7f"
  | Ident "steelblue"               -> Hexcolor "4682b4"
  | Hexcolor "d2b48c"               -> Ident "tan"
  | Hexcolor "008080"               -> Ident "teal"
  | Hexcolor "ff6347"               -> Ident "tomato"
  | Ident "turquoise"               -> Hexcolor "40e0d0"
  | Hexcolor "ee82ee"               -> Ident "violet"
  | Ident "violetred"               -> Hexcolor "d02090"
  | Hexcolor "f5deb3"               -> Ident "wheat"
  | Ident "white"                   -> Hexcolor "fff"
  | Ident "whitesmoke"              -> Hexcolor "f5f5f5"
  | Ident "yellow"                  -> Hexcolor "ff0"
  | Ident "yellowgreen"             -> Hexcolor "9acd32"
  | v                               -> v

let is_color = function
  | Hexcolor _
  | Function (("rgb" | "rgba" | "hsl" | "hsla"), _)
  | Ident "transparent"
  | Ident ("aliceblue" | "antiquewhite" | "aqua" | "aquamarine" | "azure" |
    "beige" | "bisque" | "black" | "blanchedalmond" | "blue" | "blueviolet" |
    "brown" | "burlywood" | "cadetblue" | "chartreuse" | "chocolate" | "coral" |
    "cornflowerblue" | "cornsilk" | "crimson" | "cyan" | "darkblue" | "darkcyan"
    | "darkgoldenrod" | "darkgray" | "darkgreen" | "darkkhaki" | "darkmagenta" |
    "darkolivegreen" | "darkorange" | "darkorchid" | "darkred" | "darksalmon" |
    "darkseagreen" | "darkslateblue" | "darkslategray" | "darkturquoise" |
    "darkviolet" | "deeppink" | "deepskyblue" | "dimgray" | "dodgerblue" |
    "feldspar" | "firebrick" | "floralwhite" | "forestgreen" | "fuchsia" |
    "gainsboro" | "ghostwhite" | "gold" | "goldenrod" | "gray" | "green" |
    "greenyellow" | "honeydew" | "hotpink" | "indianred" | "indigo" | "ivory" |
    "khaki" | "lavender" | "lavenderblush" | "lawngreen" | "lemonchiffon" |
    "lightblue" | "lightcoral" | "lightcyan" | "lightgoldenrodyellow" |
    "lightgrey" | "lightgreen" | "lightpink" | "lightsalmon" | "lightseagreen" |
    "lightskyblue" | "lightslateblue" | "lightslategray" | "lightsteelblue" |
    "lightyellow" | "lime" | "limegreen" | "linen" | "magenta" | "maroon" |
    "mediumaquamarine" | "mediumblue" | "mediumorchid" | "mediumpurple" |
    "mediumseagreen" | "mediumslateblue" | "mediumspringgreen" |
    "mediumturquoise" | "mediumvioletred" | "midnightblue" | "mintcream" |
    "mistyrose" | "moccasin" | "navajowhite" | "navy" | "oldlace" | "olive" |
    "olivedrab" | "orange" | "orangered" | "orchid" | "palegoldenrod" |
    "palegreen" | "paleturquoise" | "palevioletred" | "papayawhip" | "peachpuff"
    | "peru" | "pink" | "plum" | "powderblue" | "purple" | "red" | "rosybrown" |
    "royalblue" | "saddlebrown" | "salmon" | "sandybrown" | "seagreen" |
    "seashell" | "sienna" | "silver" | "skyblue" | "slateblue" | "slategray" |
    "snow" | "springgreen" | "steelblue" | "tan" | "teal" | "thistle" | "tomato"
    | "turquoise" | "violet" | "violetred" | "wheat" | "white" | "whitesmoke" |
    "yellow" | "yellowgreen") -> true
  | _ -> false
