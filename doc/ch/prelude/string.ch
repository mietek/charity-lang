
data string = list char.


def eq_string: string * string -> bool = ps => eq_list{eq_char} ps.


def explode: string -> list string = s => list{c => [c]} s.


def implode: list string -> string = s => flatten s.

