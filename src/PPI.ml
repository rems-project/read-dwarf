(** This module provide intermediate pretty printing, i.e it add all custom data structure pretty printing. No file with important code shall be added here

    The other pretty printing convenience module are:
    - PP : Pretty Printing : StdLib and external pretty printing
    - PPA : Pretty Printing all : everything
*)

include PP
include Vector.PP
include HashVector.PP
include Bimap.PP
include Reg.PP
include State.PP
