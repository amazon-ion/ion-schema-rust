HashMap::from([
    ("any", "type::{ name: any, one_of: [ blob, bool, clob, decimal,
                                    float, int, string, symbol, timestamp,
                                    list, sexp, struct ] }"),
    ("lob" , "type::{ name: lob, one_of: [ blob, clob ] }"),
    ("number", "type::{ name: number, one_of: [ decimal, float, int ] }"),
    ("text", "type::{ name: text, one_of: [ string, symbol ] }")
])
