SMLofNJ.Internals.GC.messages false;
Compiler.Control.Print.printDepth := 100;
Compiler.Control.Print.printLength := 100;
Compiler.Control.Print.stringDepth := 300;


val rootFunName = "start"

structure chLrVals = chLrValsFun(structure Token = LrParser.Token)
structure chLex = chLexFun(structure Tokens = chLrVals.Tokens)
structure chParser= Join(structure ParserData = chLrVals.ParserData
                         structure Lex=chLex
                         structure LrParser=LrParser)

structure TE = TypeExpression
structure IST = ImperativeSymbolTable

fun buildAST filename =
    let
        val infile = (TextIO.openIn filename) handle ex => (print("Unable to open file "^filename); raise ex)
        val lexer =  chParser.makeLexer (fn m =>  TextIO.inputN(infile,m))
        val print_error =
                fn (s,i:int,_) => TextIO.output
                    (TextIO.stdOut,"Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
    in
        chLex.UserDeclarations.resetLineNo();
        (#1 (chParser.parse(0,lexer,print_error,())))
            handle AST.YACCERR(msg) => (print msg; [])
        before
            TextIO.closeIn(infile)
    end
(*
val initialSymbolTable =
    let open Predefined
    in
        loadPredefinedFunctions (loadPredefinedDatatypes ST.empty)
    end
*)
fun parseAST aAST = (IST.reset();
                     Predefined.loadPredefinedDatatypes();
                     Predefined.loadPredefinedFunctions();
                     AST2CT.parseAST aAST)
fun parseFile filename = parseAST(buildAST filename)



fun st2code () = Term2Code.generateAllCode(rootFunName)


(*read file contents into a list of strings, note the order 
  of string in the list is reversed 
 *)
fun readLines(resultlist, infile) = 
    let 
        val aline = TextIO.inputLine infile
    in
        if aline = "" then resultlist
                      else readLines(aline::resultlist, infile)
    end
(*write a list of strings into stream*)
fun writeLines([], stream) = ()
  | writeLines(s::ss, stream) = (TextIO.output(stream, s); writeLines(ss,stream))

(*load the vmc init file at time of make because it's troublesome to try to 
  find the file when running the translator as a standalone program.
  this way, the string list is built into the image file
 *)
val vmcInitFile = 
    let 
        val startfile = TextIO.openIn "startup.vmc"
    in
        rev(readLines([], startfile))
        before TextIO.closeIn startfile
    end

fun code2file(codelist, filename) =
    let
        val outfile = TextIO.openOut filename
        fun outputCodeList(x::xs) = (TextIO.output(outfile, VMCCode.toString x ^ "\n");
                                     outputCodeList xs)
          | outputCodeList [] = ()

        fun genOut [] = TextIO.closeOut outfile
          | genOut ((fname,l)::ls) = (TextIO.output(outfile, ";\n; "^fname ^ "\n;\n");
                                      outputCodeList l;
                                      genOut ls)
    in
        (writeLines(vmcInitFile, outfile);
         genOut codelist)
    end

fun translate filename =
    let
        fun printerr(msg) = (print("Error at top level "^":"^msg^"\n"))
    in
        if parseFile(filename ^ ".ch") then
            if(IST.isFUN(IST.find(rootFunName))) then
                code2file(st2code(), filename^".vmc")
            else
                print("main function '"^rootFunName^"' not defined\n")
        else
            ()
    end

fun charity(execpath, []) = raise Fail("The charity function should be called with parameters")
  | charity(execpath, [x]) =
    (print("Charity to VMC Compiler, version 0.99\n");
     print("Copyright(c) 2002-2004 Charity Group, University of Calgary\n");
     print("Usage: charity inputfile outputfile\n");
     OS.Process.failure
    )
  | charity(execpath, [x,srcname]) = charity(execpath, [x, srcname^".ch", srcname^".vmc"])
  | charity(execpath, [x,srcname, dstname]) = 
    let 
        val ast= buildAST srcname
    in
        if(ast=[]) then
            OS.Process.failure
        else if parseAST ast = false then
            OS.Process.failure
        else if not (IST.isFUN(IST.find(rootFunName))) then
            (print("main function '"^rootFunName^"' not defined\n");
             OS.Process.failure
            )
        else
            (code2file(st2code(), dstname);
             OS.Process.success
            )
    end

