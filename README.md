sourceproggui
=============

A simple GUI for highlighting functions in the source text
of a Curry module.

To start the GUI:

    > curry-showsource <module name>
    
Select highlighting by commands written on stdin:

    +fun          -> highlight function "fun"
    -fun          -> remove highlighting for function "fun"
    <empty line>  -> terminate GUI
    q             -> terminate GUI
