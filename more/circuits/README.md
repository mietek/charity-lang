# Circuits User Documentation

## Table of Contents

-   [1. Introduction](#1-introduction)
    -   [1.1 About the Program](#11-about-the-program)
    -   [1.2 Start-up](#12-start-up)
-   [2. Creating and Editing Circuits](#2-creating-and-editing-circuits)
    -   [2.1 Adding Components](#21-adding-components)
        -   [2.1.1 Adding Fixed-Size Components](#211-adding-fixed-size-components)
        -   [2.1.2 Adding Abstraction Boxes](#212-adding-abstraction-boxes)
    -   [2.2 Adding wires](#22-adding-wires)
    -   [2.3 Editing a Circuit](#23-editing-a-circuit)
        -   [2.3.1 Selecting Components and Wires](#231-selecting-components-and-wires)
        -   [2.3.2 Moving Components](#232-moving-components)
        -   [2.3.3 Resizing Abstraction Boxes](#233-resizing-abstraction-boxes)
        -   [2.3.4 Deleting Components and Wires](#234-deleting-components-and-wires)
        -   [2.3.5 Setting the Type on a Wire](#235-setting-the-type-on-a-wire)
        -   [2.3.6 Removing a User-Specified Type from a Wire](#236-removing-a-user-specified-type-from-a-wire)
        -   [2.3.7 Erasing a Circuit](#237-erasing-a-circuit)
    -   [2.4 User-Defined Components](#24-user-defined-components)
        -   [2.4.1 Creating a New Component](#241-creating-a-new-component)
        -   [2.4.2 Reading New Components in from a File](#242-reading-new-components-in-from-a-file)
    -   [2.5 Saving Circuits](#25-saving-circuits)
    -   [2.6 Reading Circuits](#26-reading-circuits)
    -   [2.7 Opening and Closing Circuit Windows](#27-opening-and-closing-circuit-windows)
-   [3. Doing Proofs](#3-doing-proofs)
    -   [3.1 Moving and Resizing Components](#31-moving-and-resizing-components)
    -   [3.2 Performing Rewrites](#32-performing-rewrites)
    -   [3.3 Rewiring Thinning Links](#33-rewiring-thinning-links)
    -   [3.4 Flipping Thinning Links](#34-flipping-thinning-links)
    -   [3.5 Checking for Equality of Circuits](#35-checking-for-equality-of-circuits)
-   [4. Rewrite Rules](#4-rewrite-rules)
    -   [4.1 Creating a New Rewrite Rule](#41-creating-a-new-rewrite-rule)
    -   [4.2 Reading Rewrite Rules from a File](#42-reading-rewrite-rules-from-a-file)
    -   [4.3 Editing Existing Rewrite Rules](#43-editing-existing-rewrite-rules)
    -   [4.4 Replacing Existing Rewrite Rules](#44-replacing-existing-rewrite-rules)
    -   [4.5 Viewing Rewrite Rules](#45-viewing-rewrite-rules)
-   [5. Types](#5-types)
-   [6. Quitting the Program](#6-quitting-the-program)
-   [7. Miscellaneous Functions](#7-miscellaneous-functions)
    -   [7.1 Redrawing the Circuit](#71-redrawing-the-circuit)
    -   [7.2 Checking that a Circuit Sequentializes](#72-checking-that-a-circuit-sequentializes)
    -   [7.3 Checking the Consistency and Order of Connections](#73-checking-the-consistency-and-order-of-connections)
-   [8. Selecting a Circuit Window](#8-selecting-a-circuit-window)
-   [9. Compiling the Program](#9-compiling-the-program)
-   [10. The Source Code](#10-the-source-code)


## 1. Introduction

### 1.1 About the Program

Circuits was written for the Clean 1.1 compiler. To quote from the Clean License Conditions:

> _The Concurrent Clean Compiler and the Concurrent Clean Program Development System are developed by the Research Group on Functional Programming Languages at the University of Nijmegen (copyright University of Nijmegen, 1987–1996)._
>
> _The Clean Software can be used free of charge only if it is used for non-profit purposes in a non-commercial environment, i.e. for educational or research purposes in a non-profit institute or for personal, non-commercial use. For this kind of use it is allowed to copy the software under the condition that the complete distribution (see the Clean Distribution page) for a certain platform is copied, including this license condition, copyright notice, language manual, help file, the compiler, the code generator and the development system. Under the same conditions it is allowed to re-distribute the system. We appreciate it very much if you would inform us when you copy or redistribute the system._
>
> _For ANY use of Clean with a commercial purpose or in a commercial environment a commercial license is needed._

For this reason, we are making only the source code available. To use the program it is necessary to obtain a copy of the Clean 1.1 compiler and then compile the program (instructions are given in [Section 9, “Compiling the Program”](#9-compiling-the-program)). Clean compilers are available for several different platforms, but we have tested the program only with the SunOS version (using both the OpenLook and XView toolkits). We were unable to compile it for the Mac.

A list of the program’s modules and files, with a brief description of each module, is given in [Section 10, “The Source Code”](#10-the-source-code).

Note that the Clean 1.1 compiler is still in development and has some bugs. If the program crashes due to a segmentation fault or bus error, it is almost certainly due to a bug in the compiler, but if it crashes with a message to the effect that some rule doesn’t match, the bug is most likely in the program itself.

This program is based on the circuits (or proof nets) described in “Natural deduction and coherence for weakly distributive categories” ([final PDF](../../doc/pdf/more/blute-1996.pdf), [PDF](../../doc/pdf/more/blute-1991.pdf)) by R. F. Blute, J. R. B. Cockett, R. A. G. Seely and T. H. Trimble and “Proof theory for full intuitionistic linear logic, bilinear logic, and MIX categories” ([PDF](../../doc/pdf/more/cockett-1997b.pdf)), by J. R. B. Cockett and R. A. G. Seely. Sequentialization, rewiring and rewrites are also covered in these papers. (This program does its sequentialization in FILL.) The user is assumed to be familiar with these circuits.


### 1.2 Start-up

On startup of the program you will have a menu, a toolbox, and one window in which to create a circuit. (You may also open a second circuit window by selecting “Open Window” from the File menu.) At this point, the program is in edit mode (as opposed to proof mode, explained in [Section 3, “Doing Proofs”](#3-doing-proofs)), allowing you to create and edit circuits. For a description of the toolbox, see [Section 2, “Creating and Editing Circuits”](#2-creating-and-editing-circuits).


## 2. Creating and Editing Circuits

To create a circuit, you must start by adding some components, which you can then connect by wires. The currently selected button in the toolbox (which will have a thicker border than the others) determines the behaviour of the mouse in the circuit window, i.e. whether you can add components, add wires, or select components or wires with it. You may change the mouse behaviour by clicking on the appropriate toolbox button. Clicking on any of the first 15 buttons in the toolbox (from left to right, starting with the top row) will allow you to add components (identified by the pictures on the buttons), clicking on the 16th button will allow you to add wires between components, and clicking on the last button (bottom right) will allow you to select components and wires to be moved, deleted, etc.


### 2.1 Adding Components

The available components (in the order in which they appear in the toolbox) are input terminals, output terminals, tensor eliminations, tensor introductions, sum eliminations, sum introductions, lollys, abstraction boxes, user-defined (or generic) components, unit introductions, left unit eliminations, right unit eliminations, counit eliminations, left counit introductions and right counit introductions. Input terminals are used to anchor the starting points of input wires to the circuit, and output terminals are used to anchor the ending points of output wires. Terminals by default have a variable as a type, and so have no impact on the other types in the circuit; the remaining components are logical components and introduce a type discipline.


#### 2.1.1 Adding Fixed-Size Components

All components except the abstraction box can be added by selecting the appropriate toolbox button and then clicking on the desired location of the component in the circuit window. For terminals, the mouse position indicates the point centred at the base of the terminal. For tensor elimination, tensor introduction, sum elimination, sum introduction, lolly, unit introduction, counit elimination and user-defined components, the mouse position indicates the centre of the component. For the left and right unit elimination and left and right counit introduction components, the mouse position indicates the centre of the lasso loop (attached by an arc to the unit or counit). When you click on the position for a user-defined component, a dialog box will appear that allows you to select the component from a list.

_Problems:_

-   If the component does not appear when you click, it must be because the component would overlap another component if placed at that position. Try a different position away from other components, or move other components out of the way. (A component “overlaps” another component if the rectangle enclosing it (but not its connections) overlaps either the rectangle enclosing the other component or, if the other component is a box, one of its border areas. In the case of the unit elimination and counit introduction components, the enclosing rectangle includes the arc and lasso loop, so these components may overlap other components (as far as the program is concerned) even when they don’t appear to overlap. Wires, however, are allowed to cross anything.

-   User-defined components cannot be added until they have either been created from scratch or read in a from a file. See [Section 2.4, “User-Defined Components”](#24-user-defined-components) for details. There are no user-defined components available at the beginning of any program session.

-   If you select one user-defined component from the list but another one appears in the circuit, it seems that the best thing to do is to try again immediately (without deleting the component you just added first). I believe that this problem is due to a bug in the Clean 1.1 compiler for X-Windows.


#### 2.1.2 Adding Abstraction Boxes

Abstraction boxes may vary in size, and their initial size must be set when they are created. To add an abstraction box, select the appropriate toolbox button (second from the right on the top row), then press the mouse button down over one of the corner positions for the box and stretch the box (with the mouse button down) to the desired size, then release the mouse button. A box may be placed so that its border surrounds other components that have already been placed, in which case these components will become part of the box’s internal circuit, but the border regions of the box should not overlap any other components.

_Problems:_

-   If the box disappears when the mouse button is released, it may be because it would have overlapped other components, or it may be because the box’s dimensions were smaller than required. Try making the box bigger and avoid placing the box so that its borders are too close to other components, especially in the top and bottom middle areas of the border. (The box can be resized after it is placed.)


### 2.2 Adding wires

Wires are added between input and output connections of existing components, in most cases indicated by short lines extending from the component. (Input connection lines extend up from the component and output connection lines extend down from the component.) For thinning links (left and right unit elimination and left and right counit introduction), there is one connection attached to the unit or counit at the end of the lasso arc, and two more connections (an input and an output connection which are not indicated by lines) in the lasso loop at the other end of the lasso arc.

To add a wire, first select the appropriate toolbox button (second from the end on the bottom row), then press the mouse button down over one of the connection points and drag the end of the wire (still holding the mouse button down) to the other connection point, and release the mouse button.

A box may have a wire connecting its internal output to its internal input, but apart from that no component may be connected to itself. Wires may not be added if they create a cycle in the circuit or if the types of the two connection points cannot be unified, and only one wire may be attached to a connection.

Note that adding a wire will cause the circuit’s types to change.

_Problems:_

-   If a wire fails to appear when you click at a connection point and then drag the cursor with the mouse button down, you didn’t click close enough to the first connection point (or the connection point is already connected to a wire). In general, you can click anywhere in a small rectangular region surrounding the connection line, but the region varies in size depending on the component and the connection. Clicking directly on the line should always work, but clicking at the end of the line sometimes won’t if you aren’t quite close enough. For thinning links, click on or near the top half of the lasso loop to make an input connection and click on or near the bottom half of the lasso loop to make an output connection.

-   If the wire disappears after you have dragged its end to the other connection point and released the mouse button, it may be because you didn’t get close enough to the second connection (see the paragraph above for details), it may be because you are trying to connect two inputs or two outputs, or it may be because the wire would introduce a cycle into the circuit. (If the two connections don’t unify, a message will be displayed.)


### 2.3 Editing a Circuit

#### 2.3.1 Selecting Components and Wires

Components and wires may need to be selected for various reasons. First make sure that the Selection button (the bottom-right button) is selected in the toolbox. To select most components, you should click with the mouse button over the centre area of component, excluding connection lines. When the component is selected, a rectangle will appear around it. To select an abstraction box, click on one of its borders, on the arc at the internal output connection, or on the lolly in the middle of the lower border. A rectangle with four rectangular selection areas at the corners will appear around the box. To select a wire, click on or near the wire. Two lines will appear parallel to the selected wire.


#### 2.3.2 Moving Components

To move a component, first make sure the Selection button is selected, then select the component with the mouse and drag it to its new position (still holding the mouse button down), then release the mouse button. The wires attached to the component will not move with it, but they will snap into place once it has been moved. (When an abstraction box is moved, its contents move with it.) Components may be moved in and out of abstraction boxes (provided that this does not result in a cycle).

**NOTE:** It is possible to move a component outside the picture domain of the window (beyond the area that can be viewed by scrolling), but you should not do this, since a component that is not in the picture domain can not selected or deleted without erasing the whole circuit.

_Problems:_

-   If the component snaps back to its original position when you release the mouse button, either it overlaps another component or moving it into or out of a box introduced a cycle in the circuit. If no cycle is being introduced, try moving the component again to a position farther away from other components, or move the other components. See [Section 2.1.1, “Adding Fixed-Size Components”](#211-adding-fixed-size-components), for details on how components overlap.


#### 2.3.3 Resizing Abstraction Boxes

To resize an abstraction box, first make sure the Selection button is selected, then select the box (releasing the mouse button). Now press the mouse button down over one of the four rectangular selection areas at the corners of the box and drag it (with the mouse button still down) to the new corner position, then release the mouse button. The corner of the box will follow the mouse cursor unless this would result in the box becoming too small in one or both dimensions. Wires that are attached to the box will not move with it, but will snap into place once it has been resized.

The abstraction box may be resized so that its border encloses new components and/or doesn’t enclose components that it did enclose before, as long as no cycles in the circuit result, but the border may not overlap any other components (see [Section 2.1.1, “Adding Fixed-Size Components”](#211-adding-fixed-size-components), under “Problems” for details on how components overlap).

_Problems:_

-   If the border snaps back to its original size after the mouse button is released, either the border of the box overlapped some components, or a cycle would have resulted in the circuit from the resizing.


#### 2.3.4 Deleting Components and Wires

Select the component or wire to be deleted (see [Section 2.3.1, “Selecting Components and Wires”](#231-selecting-components-and-wires), for details), then select “Delete” from the Edit menu. If a component with wires attached is deleted, the wires are also deleted (along with any user-specified types that they may have).

Note that the circuit’s types will change when a wire or component is deleted.


#### 2.3.5 Setting the Type on a Wire

To set a type, select the wire whose type you want to set, then choose “Set Type” from the Edit menu. A dialog box will appear in which you may enter a new type, which must unify with the remainder of the circuit. You may enter user variables, constants (enclosed in double-quotes), user-defined functions (followed by a list of type arguments in parentheses, separated by commas), units (as `Unit`), counits (as `Counit`) and the predefined infix functions for products (`(*)`), sums (`(+)`) and implications (`(-o)`), but regular variables and numeric user variables cannot be entered. User variables, constants, and the names of user-defined functions must contain only upper- and lower-case letters (which are treated as different) and underline characters.

_For example:_

    My_Func(My_Var_A (*) "My_Const", Unit (+) (Counit (-o) My_Var_B))

Note that the program will not prevent you from defining multiple functions with the same name but different numbers of arguments. If you do this, these functions will be treated as different from each other.

When you have placed a user-defined type on a wire, the wire will be drawn with a short line across the centre.

In a circuit, constants and user variables are effectively the same — neither can be replaced by a substitution when retyping the circuit. However, if user variables are included in a user-defined rewrite rule, they will be replaced with the matching circuit types when the rewrite is performed, except that any variables in the matching types will become numeric user variables. Note that a rewrite may also introduce new “user-defined” types in the circuit, in which case any regular variables in types that become user-defined will become numeric user variables.

See [Section 5, “Types”](#5-types), for more details on the types.

_Problems:_

-   If the type you’ve entered does not unify, remember that user variables (whether entered by you when setting a wire’s type or introduced into the circuit by a rewrite), constants, units and counits cannot be replaced by another type. You may be able to solve the problem by removing or changing a user-specified type elsewhere in the circuit.


#### 2.3.6 Removing a User-Specified Type from a Wire

To remove a user-specified type, select the wire, then choose “Remove type” from the Edit menu. The whole circuit will be retyped and the wire’s type will revert to the most general type possible given the other types in the circuit.


#### 2.3.7 Erasing a Circuit

To erase a circuit completely, first make sure that the circuit’s window is selected (see [Section 8, “Selecting a Circuit Window”](#8-selecting-a-circuit-window)), then select “Erase” from the Edit menu. This allows you to begin a new circuit from scratch.


### 2.4 User-Defined Components

In addition to the built-in components, the user may create new named components. These components will be treated as “boxable” components (such as the unit and counit components, the tensor introduction component, etc.) when the circuit is sequentialized. In any program session, user-defined components must be created or read in from a file before they can be used.


#### 2.4.1 Creating a New Component

To create a new component, follow these steps:

1. Select “Create Component” from the New Components menu. A dialog box will appear in which you enter the name of the component (which will be displayed on the component) and the number of inputs and outputs for the component. When you have entered the information in the initial dialog box, click on the Continue button. If you have given the component the same name as an existing component (that has been defined or read in from a file in the current session), you will be given the choice of replacing the existing component with the new one or of cancelling the creation of the component. (If you are replacing an existing component, the old component will *not* be overwritten in its file. You will be prompted for a file in which to save the new component, which will be appended at the end even if the file contains another component of the same name.)

    **NOTE:** If your current circuit(s) already include the old component, it will remain in the circuit(s) and continue to have the same name and type properties, although it may look exactly the same as the new component, if the new component has the same numbers of inputs and outputs.

2. When you are finished entering the component’s name and numbers of inputs and outputs, a small window will appear with a picture of your new component with its inputs and outputs all connected to terminals, as well as a dialog box that allows you either to continue or cancel the creation of the new component. At this point, you need to set the types on the new component, which by default are all set to different variables. To set the type of one of the component’s connections, click on the attached wire and enter the new type in the dialog box as described in [Section 2.3.4, “Setting the Type on a Wire”](#234-setting-the-type-on-a-wire). (If you are resetting a type that you have already set, the old type will appear in the entry field.) Wires whose types have been set will have short crossbars on them. You should usually set the types for all the wires, since any remaining wires will have arbitrary variable types. When the same variable should appear in the types for two or more connections, make sure that you use exactly the same variable name in each case (lower- and upper-case letters are considered to be different). User variables in the component’s types will be instantiated with ordinary variables when the component is placed in a circuit.

3. When you have set all the component’s types, click on the Continue button. A file-selection dialog box will appear to allow you to select a file in which to save the new component. When you have selected a file, the component will be saved at the end of the selected file (which should only contain other components) and can then be added to a circuit. The new component will *not* overwrite the contents of an existing file, although the file-selection box may imply that it does by asking you whether you want to overwrite the file. (The behaviour of the file-selection box is beyond the control of the program.)


#### 2.4.2 Reading New Components in from a File

To read the components in from a file, choose “Read Component” from the New Components menu, then select the component’s file from the file-selection dialog. If a component with the same name has already been created or read in from a file in the current session, you will be prompted to replace the old component or not read in the new component.

**NOTE:** If you are replacing a component that has already been used in current circuits, the old version will remain in the circuits and will continue to have the same name and type properties, although it will look identical to the new component if it has the same numbers of inputs and outputs.

_Problems:_

-   If not all the components are read in from the file, some part of the file did not contain a valid component. If you save a new component to the end of a file containing anything other than components, you will not be able to read it.


### 2.5 Saving Circuits

To save a circuit in a file, first make sure the window containing the circuit to be saved is selected (see [Section 8, “Selecting a Circuit Window”](#8-selecting-a-circuit-window)), then select “Save Circuit” from the File menu. A file-selection dialog will open to allow you to select or enter the file in which to save the circuit. If an existing file is selected, its contents will be replaced by the circuit. When the circuit has been saved, the file name will appear in the window’s caption.

**NOTE:** The program does not keep track of whether the circuit has been saved since the last time it was edited, and it will *not* prompt you to save your changes if you decide to exit the program.


### 2.6 Reading Circuits

To read a circuit from a file into a window, first make sure that the window that you want to read the circuit into is selected (see [Section 8, “Selecting a Circuit Window”](#8-selecting-a-circuit-window)), then select “Read Circuit” from the File menu. A file-selection dialog will open to allow you to select or enter the name of the file to be read. If there is a circuit already in the selected window, you will be asked to confirm that it should be replaced. When the circuit has been read into the window, the file name will appear in the window’s caption.

**NOTE:** If you read in a circuit that contains user-defined components, these components will not automatically be added to the list of components available in the current program session. If any of these components has the same name as a user-defined component that is available in the current session, it will not necessarily be identical to the other component, even if it has the same number of inputs and outputs and looks the same.

**NOTE:** The program does not keep track of whether the circuit has been saved since the last time it was edited, and it will *not* prompt you to save your changes if you decide to exit the program.


### 2.7 Opening and Closing Circuit Windows

You may have at most two circuit windows open. To open another circuit window, select “Open Window” from the File menu. To close a window, first make sure the window you want to close is selected (see [Section 8, “Selecting a Circuit Window”](#8-selecting-a-circuit-window)), then select “Close Window” from the File menu.

**NOTE:** If you close a window, its circuit will be permanently lost unless you have already saved it in a file. You will be asked for confirmation before the window is closed, however.

The caption of each window includes its name, which is either “Window 1” or “Window 2”. These names are used when confirming various actions that involve one of the windows.


## 3. Doing Proofs

Once your circuits are complete, you may enter proof mode to apply rewrites and rewire thinning links. To do this, select “Proof Mode” from the Mode menu. Note that your circuit or circuits must all sequentialize (in FILL) before you can enter proof mode, and components must be completely connected by wires. The circuit’s input and output wires must be connected to terminals and no terminals may be inside any boxes.

Once you are in proof mode, a new toolbox will appear with two buttons, the first for rewiring thinning links and the second for selection of items to move or resize them or to perform rewrites. No other changes to the circuits will be possible until you return to edit mode.


### 3.1 Moving and Resizing Components

Components may be moved in proof mode exactly as in edit mode (see [Section 2.3.2, “Moving Components”](#232-moving-components)), except that the circuit must still sequentialize after each move, and the order of the input and output terminals (as determined by their horizontal positions) cannot change. If an attempt is made to move a component to a position that would result in the circuit not sequentializing, the component will snap back into its original position when the mouse button is released. If an attempt is made to move a terminal horizontally past another terminal of the same type, the terminal will not follow the mouse cursor. Abstraction boxes can also be resized as in edit mode (see [Section 2.3.3, “Resizing Abstraction Boxes”](#233-resizing-abstraction-boxes)) as long as the circuit still sequentializes after. If an attempt is made to resize a box so that the circuit no longer sequentializes, the box will snap back to its original size when the mouse button is released.


### 3.2 Performing Rewrites

All rewrites are identified by a “live wire” on the left-hand side of the rule. To perform a rewrite in a circuit, select the appropriate wire and then select the rewrite rule from the list in the “Do Rewrite” menu. The part of your circuit to be rewritten must match the left side of the rewrite rule (but the order of inputs and outputs does not matter), and the input and output types must match as well (they must be no more general than the rule’s types), although any user variables in the right side of the rewrite will be treated as ordinary variables for the purpose of checking that the types match. (The input and output types of a rewrite rule are determined by unifying the input and output types of the two sides of the rule.) The circuit must also still sequentialize after the rewrite is performed.

When a rewrite is performed, new user-specified types may be introduced into the circuit (these wires will have short crossbars when the rewrite is done). Any variables in types which become user-specified or any variables in types that replace user variables in the right-side of the rule will be replaced with new numeric user variables, which behave like other user variables in the circuit. (See [Section 5, “Types”](#5-types) for more details on the types.)

_Problems:_

-   If it says that the selected part of the circuit does not match the selected rewrite rule, you may have selected the wrong “live wire”. You can view the two sides of the rewrite rule by selecting the rewrite from the View Rewrite menu. Two display windows will appear in which the live wire on the left side will be selected. You can close these display windows by choosing “Close Rewrite Windows” from the View Rewrite menu.

-   If it says that the right side of the rule overlaps the circuit, try moving components surrounding the ones involved in the rewrite as far as possible out of the way to make room for the new components. If the rewrite is user-defined, it may also be worthwhile to edit the rewrite to make the right side more compact (see [Section 4.3, “Editing Existing Rewrite Rules”](#43-editing-existing-rewrite-rules)).


### 3.3 Rewiring Thinning Links

To rewire a thinning link, make sure the Rewiring button is selected in the toolbox (this is the first button), select the thinning link to be rewired (with the Rewiring button selected, it will be impossible to select anything but a thinning link), and then click on the wire that you would like the thinning link to be moved to. You may not rewire a thinning link to any of the three wires that are connected to it already, but otherwise a thinning link may be rewired to any wire within its empire (i.e. within the same sequent box when all of the circuit that doesn’t have a direct connection down to the unit or up to the counit of the thinning link is sequentialized as far possible). (The program will tell you if you have attempted to rewire a thinning link outside its empire, or if the thinning link’s new position would overlap another component.)

_Problems:_

-   If nothing happens when you click on the wire, you must not have been quite close enough to it, or you do not have a thinning link selected.

-   If the new position of the thinning link (with the small lasso loop at the end of the lasso arc centred at the selected mouse position) overlaps another component, you may either try to rewire it again slightly farther away, or move the other component(s) after first selecting the Selection button in the toolbox (this is the second button).


### 3.4 Flipping Thinning Links

You may “flip” unit and counit thinning links from left to right when either the Rewiring or the Selection button is selected by selecting the thinning link, then choosing “Flip Thinning Link” from the Flip Link menu. The thinning link is flipped within the same rectangular area (to avoid overlaps), so the endpoints of the wires attached to the thinning link will be moved.


### 3.5 Checking for Equality of Circuits

You can check that the circuits in the two circuit windows are structurally the same by selecting “Check Equality” from the Verify menu. Note that input and output terminals are matched from left to right in order. (This function does not check for equivalent wire types.)


## 4. Rewrite Rules

There are fourteen built-in rewrite rules and new rewrite rules may also be defined by the user. Only the built-in rewrite rules are available at the beginning of any program session, but new ones may then be created or read from files. The built-in rewrite rules are the rules for tensor reduction, sum reduction, left unit reduction, right unit reduction, left counit reduction, right counit reduction, box reduction, tensor expansion, sum expansion, left unit expansion, right unit expansion, left counit expansion, right counit expansion, and box expansion.


### 4.1 Creating a New Rewrite Rule

To create a rewrite rule:

1. Open both windows (in edit mode), then put the left- and right-side circuits in the two windows (it doesn’t matter which windows the two sides go into). The left side must consist of a single connected circuit, since a single wire is used to identify it in the circuit, and all components except the input and output terminals in the right side should be moved together as compactly as possible to avoid problems with overlapping components when the rewrite rule is applied. The inputs and outputs of the two sides are matched by the horizontal positions of the input and output terminals, and their types must unify with no substitution of user variables. (The input and output types of the rule will be the unified types.)

    **NOTE:** A numeric user-variable (introduced by a rewrite) on one side will never unify with a numeric user-variable on the other side, even if they have the same value. In these cases, the type should be changed on each side to have the same regular user variable (stored as a string, as entered by the user) instead. See [Section 5, “Types”](#5-types), for more details about the the types.

2. Select the “live wire” (which will be used to identify the position at which the rewrite rule is to be applied in the circuit) in the left side of the rule, then choose “Save New Rewrite” from the File menu. The live wire must have both endpoints at the uppermost level of the rewrite rule (i.e. it cannot enter or leave a box).

3. A dialog box will open. Enter the new rewrite rule’s name (the name that will identify the rewrite rule in the rewrite menus), then click on the Continue button, or click on the Cancel button to cancel saving the new rewrite rule.

4. A file-selection dialog will open. Select or enter the name of the file in which to save the new rewrite rule, then click on the Continue button, or click on Cancel to cancel saving the rule (if you cancel, the rule will not be added to the rewrite menus). The rewrite rule will be saved at the end of the selected file (which should contain only other rewrite rules) and then added to the bottom of all rewrite menus. The new rewrite rule will *not* overwrite the contents of an existing file, although this may be implied by the file-selection dialog.


### 4.2 Reading Rewrite Rules from a File

To read a rewrite rule that has been saved, select “Read Rewrite” from the file menu. A file-selection dialog box will appear from which you may select the file. When the rewrite rule has been read, it will be added to the bottom of all rewrite menus.

_Problems:_

-   If not all the rewrite rules are read, it is because some part of the file did not contain a valid rewrite rule. If you added a rewrite rule to the end of a file that contained anything other than other rewrite rules, you will not be able to read it.


### 4.3 Editing Existing Rewrite Rules

To edit a rewrite rule that has already been defined in a program session (including built-in rewrites), make sure both circuit windows are open (in edit mode), then select the rule from the Edit Rewrite menu. The left side of the rule will replace the circuit in Window 1 and the right side will replace the circuit in Window 2. You may edit them just as you would edit any other circuits. User-defined rewrites may then be resaved, replacing the original rewrite, by following the procedure described in [Section 4.4, “Replacing Existing Rewrite Rules”](#44-replacing-existing-rewrite-rules).

Note that the box-reduction rewrite is different from other rewrites, because the box’s internal circuit is not defined by the rule. When this rewrite is edited, a special component represented by a box with one input and output is put in the place of the box’s internal circuit. If this component is used in the left side of a user-defined rewrite rule, however, it will be treated like any other component, and must match an identical component in the circuit if the rewrite rule is to be used.


### 4.4 Replacing Existing Rewrite Rules

To replace an existing rewrite rule that has been defined in the current program session, the procedure is the same as when creating a new rewrite rule (described in [Section 4.1, “Creating a New Rewrite Rule”](#41-creating-a-new-rewrite-rule)), except that instead of selecting “Save New Rewrite” from the File menu, you select the rewrite to be replaced from the Save Rewrite menu, and you will not be prompted for the rewrite’s name. You will be prompted for the file in which it is to be saved, however.

You can first read the old rewrite rule into the circuit windows to be edited, if you wish, by following the procedure described in [Section 4.3, “Editing Existing Rewrite Rules”](#43-editing-existing-rewrite-rules).


### 4.5 Viewing Rewrite Rules

You may view a rewrite rule at any time without placing it into the circuit windows by selecting the rule from the View Rewrite menu. Two display windows will appear showing the two sides of the rewrite, with the live wire selected on the left side. You cannot edit the circuits in these windows. To view another rewrite rule, just select it from the View Rewrite menu, and it will replace the rule currently being displayed. To close the display windows, select “Close Rewrite Windows” from the View Rewrite menu.


## 5. Types

Types in the program can be variables, which are internally identified by integers; user variables specified by the user when setting a wire’s type (see [Section 2.3.4, “Setting the Type on a Wire”](#234-setting-the-type-on-a-wire)), which are identified by strings; numeric “user” variables introduced in a rewrite (see [Section 3.2, “Performing Rewrites”](#32-performing-rewrites)), which have the same properties as the other kind of user variable but are internally identified by integers; units; counits; user-defined constants, which are identified by strings; products of two types; sums of two types; implications of two types, and user-defined functions of one or more types.

To view the type on any wire in a circuit (in either edit or proof mode), make sure the Selection button is selected in the toolbox (this is the last button), click on the wire, and then select “Show Wire Type” from the Verify menu. Variables are displayed as strings beginning with an apostrophe, user variables (entered by the user) are displayed just as they were entered, numeric user variables are displayed as a string beginning with an asterisk, units are displayed as `Unit`, counits are displayed as `Counit`, and user-defined constants are displayed in quotes as entered. Products are displayed as `(<type1> (*) <type2>)`, sums are displayed as `(<type1> (+) <type2>)`, implications are displayed as `(<type1> (-o) <type2>)`, and user-defined functions are displayed as `<function name>(type1, ...)`. All types should be entered as they are displayed when setting the type on a wire, except variables and numeric user variables, which cannot be included when a type is entered by the user. User variables, constants, and the names of user-defined functions should contain only upper- and lower-case letters (which are treated as different) and underline characters.

_For example:_

    My_Func((My_Var (*) 'A), "My_Const", (Unit (+) (Counit (-o) *B)))

(`'A` is a variable assigned by the program and `*B` is a numeric user variable
introduced by a rewrite).


## 6. Quitting the Program

To quit, select “Quit” from the File menu.


## 7. Miscellaneous Functions

### 7.1 Redrawing the Circuit

Occasionally, the program may fail to redraw all of a circuit after it has been covered and then uncovered by another window. When this happens, you can redraw the circuit by making sure that the window to be redrawn is selected (see [Section 8, “Selecting a Circuit Window”](#8-selecting-a-circuit-window)), then selecting “Redraw” from the Edit menu.

This function is also useful in debugging. Since updates to the circuit stored internally occur independently from updates to the picture, it is possible that the picture might not accurately display the circuit if there is a bug in the program. Redrawing the picture will allow you to see exactly the components and wires that are currently in the circuit, although it won’t give you their order in the circuit.


### 7.2 Checking that a Circuit Sequentializes

To check that a circuit sequentializes without switching to proof mode, make sure the window containing the circuit is selected (see [Section 8, “Selecting a Circuit Window”](#8-selecting-a-circuit-window)), then select “FILL Sequentialize” from the Verify menu. This will tell you if the circuit sequentializes or not and show you what it sequentializes to if it does.

In proof mode the circuit should always sequentialize, but you may still select this function.


### 7.3 Checking the Consistency and Order of Connections

This function is useful only in debugging. If you wish to check the wire IDs that are stored on component outputs in the circuit all correspond to wire IDs stored on the inputs of components that are later in the circuit list, select “Check Connections” from the Verify menu. It should always say that the circuit is properly connected if the program has been working correctly.


## 8. Selecting a Circuit Window

A number of functions will operate only on the “current” circuit window. At startup, the current window is the open circuit window, and thereafter the current window is the circuit window that was clicked in last. If the circuit window that was clicked in last has since been closed, there will be no current window. Note that clicking in the menu or toolbox will not have any effect on which circuit window is current.

To make a circuit window current, you should first select an appropriate toolbox button to avoid drawing in the window (i.e. the Wire or Selection button (the last two on the bottom row) when in edit mode, or either button when in proof mode), then click in the window you want. (If you do inadvertently add another component to the window, however, you can easily delete it.)


## 9. Compiling the Program

You may obtain a Clean 1.1 compiler from [http://clean.cs.ru.nl/](http://clean.cs.ru.nl/). This address has a table of the available compilers with links to do the installation.

Once you have installed the compiler, you can compile this program for SunOS in the directory in which you have installed the files by typing `make <link-options file>` at the prompt, where the link-options file is one of `olit`, `olits`, `x`, `xs`, `xv`, or `xvs`. The first two (`olit` and `olits`) are for OpenWindows only, and the remainder are for X-Windows, although they can be used for OpenWindows as well. Apart from the choice of OpenWindows or X-Windows, it doesn’t seem to make much difference which one you choose, but you might try using a different one if you have had problems with the program crashing.

The program may take 5 minutes or so to compile, and the executable will be called `circuits`. To remove all files that can be recreated by recompiling, type `make cleanup` at the prompt.

Occasionally, I have gotten the message `ld.so: libCleanIO.so.1: not found` when I tried to run the program. I’m not sure what causes this, but I’ve always been able to solve it by recompiling (relinking) the program.

To compile the program for any other operating system, you should refer to the Clean documentation. The program’s main module is called `circuits`. (We were unsuccessful in an attempt to compile the program for the Mac, and haven’t tried compiling it for any other operating system.)


## 10. The Source Code

The program consists of 17 modules (35 files) as follows:

-   **`circuits`** (the main module, in [`circuits.icl`](circuits.icl)):

    This module contains the `Start` function of the program and includes all code for the creation and direct manipulation of the menu, toolboxes, windows and dialog boxes, including the mouse event handler for the circuit windows, `HandleMouse`, and all top-level menu functions. It also contains a variety of other code, including code to:

    - add a new connection between two components (`AddWire` and associated functions), checking that the connection is valid,
    - find the selected wire or component in the circuit from a mouse position (`GetSelection` and associated functions),
    - change a component’s position in a circuit after a move in edit mode (`ChangeComponent` and associated functions) and proof mode (`ChangeComponentInProof` and associated functions), checking that the move is valid,
    - move the endpoints of the wires of a component that has been moved (`MoveWires` and `MoveWire`),
    - collect and remove the components inside a newly added abstraction box from the circuit (`GetInternalComponents`),
    - change the size of a box’s border rectangle based on the previous and current mouse positions and the sizing corner selected (`ResizeBox`),
    - reorganize the circuit after a box has been resized and check that the sizing was valid in edit mode (`ChangeSizedBox` and associated functions) and proof mode (`ChangeSizedBoxInProof` and associated functions),
    - make a new component based on the edit type and mouse position (`MakeComponent`),
    - delete a component from the circuit (`DeleteComponent` and associated functions), and
    - delete a wire from the circuit (`DeleteWire` and associated functions),

-   **`circuitDefs`** ([`circuitDefs.icl`](circuitDefs.icl) and [`circuitDefs.dcl`](circuitDefs.dcl)):

    Contains many of the datatype definitions for the program (`Circuit`, `Component`, `CompSpecifics`, `Placement`, `ConnectionInfo`, `ConnectionType`, `Wire`, `ComponentID`, `ConnectionID`, `WireID`, `EditMode`, `EditType` and `ErrorVal`).

-   **`insert`** ([`insert.icl`](insert.icl) and [`insert.dcl`](insert.dcl)):

    Contains code used to insert a component into a circuit while ensuring that no cycles result (`InsertComponent`, `InsertComponentInProof`, and `InsertUnboxed`), and code to determine whether one component has any connection down to another (`ConnectsDown`). (`InsertComponentInProof` also checks for sequentialization.)

-   **`unify`** ([`unify.icl`](unify.icl) and [`unify.dcl`](unify.dcl)):

    Contains code used to unify two types (`Unify`), match two types (`MatchTypes`), make variable substitutions in a type (`SubsIntoType`), compare the constructors of two types (`SameConstructor`), or get the type arguments of a type (`Args`), as well as the `Substitution` datatype definition.

-   **`typeDefs`** ([`typeDefs.icl`](typeDefs.icl) and [`typeDefs.dcl`](typeDefs.dcl)):

    Contains the `Type` and `WireType` datatype definitions, as well as some utility functions for types (`WireTypeString`, `TypeString`, and `EqualTypes`).

-   **`parseType`** ([`parseType.icl`](parseType.icl) and [`parseType.dcl`](parseType.dcl)):

    Contains the code used to parse a type entered by the user (`ParseType`).

-   **`typeCircuit`** ([`typeCircuit.icl`](typeCircuit.icl) and [`typeCircuit.dcl`](typeCircuit.dcl)):

    Contains the code to retype a circuit after the deletion of a component or wire, or after a user-specified type has been removed (`Retype`).

-   **`generics`** ([`generics.icl`](generics.icl) and [`generics.dcl`](generics.dcl)):

    Contains some of the code used in the creation and use of generic (user-defined) components (`GetGenericName`, `MakeNewGeneric`, `SetGenericTypes`, `MakeGenericComp`, `SaveGeneric`, and `ReadGeneric`).

-   **`sequent`** ([`sequent.icl`](sequent.icl) and [`sequent.dcl`](sequent.dcl)):

    Contains the code used to check that a circuit sequentializes in FILL (`FILLSequentialize`), code to sequentialize a circuit as far as possible, while checking for fatal errors (`FILLSequentAux`), and code used to display the sequent string of a fully sequentialized circuit (`SequentString`).

-   **`rewire`** ([`rewire.icl`](rewire.icl) and [`rewire.dcl`](rewire.dcl)):

    Contains the code used to rewire a thinning link from one wire to another (`Rewire`).

-   **`rewriteDefs`** ([`rewriteDefs.icl`](rewriteDefs.icl) and [`rewriteDefs.dcl`](rewriteDefs.dcl)):

    Contains the datatype definitions for rewrites (`Rewrite`) as well as the `WireSub` datatype definition and the definition for `dummyRewrite`.

-   **`stdRewrites`** ([`stdRewrites.icl`](stdRewrites.icl) and [`stdRewrites.dcl`](stdRewrites.dcl)):

    Contains all the standard rewrites (`tensorReduct`, `sumReduct`, `unitReductLeft`, `unitReductRight`, `counitReductLeft`, `counitReductRight`, `tensorExp`, `sumExp`, `unitExpLeft`, `unitExpRight`, `counitExpLeft`, `counitExpRight`, and `boxExp`) and information used in the “reconstruction” of the box-reduction rewrite, which is a special case, for viewing or editing (`boxReductLiveWireID`, `boxReductLeftSide`, `boxReductLeftWires`, `boxReductLeftCompID`, `boxReductLeftNextWireID`, `boxReductLeftNextVar`, `boxReductRightSide`, `boxReductRightWires`, `boxReductRightCompID`, `boxReductRightNextWireID`, and `boxReductRightNextVar`).

-   **`rewrite`** ([`rewrite.icl`](rewrite.icl) and [`rewrite.dcl`](rewrite.dcl)):

    Contains the code used to create a new rewrite (`CreateRewrite`), perform a rewrite (`DoRewrite`) or perform the box-reduction rewrite (`DoBoxRewrite`).

-   **`circuitWireMatch`** ([`circuitWireMatch.icl`](circuitWireMatch.icl) and [`circuitWireMatch.dcl`](circuitWireMatch.dcl)):

    Contains the code used to compare two circuits starting from a common wire, while keeping track of wires that connect above or below the circuit and pairing wire IDs from the two circuits (`MatchCircuits`). (Note: Does not check for equivalency of types.)

-   **`circuitSave`** ([`circuitSave.icl`](circuitSave.icl) and [`circuitSave.dcl`](circuitSave.dcl)):

    Contains the code used to save a circuit to a file (`SaveCircuit`) and read it from a file (`ReadCircuitFile`), including code used to write and read various parts of a circuit file (`WriteCircuit`, `WriteComponent`, `WriteConnects`, `WritePoint`, `WriteWireType`, `WriteTypes`, `ReadStartMark`, `ReadInt`, `ReadCircuit`, `ReadComponent`, `ReadPoint`, `ReadConnects`, `ReadConnect`, `ReadWireType`, and `ReadTypes`).

-   **`rewriteSave`** ([`rewriteSave.icl`](rewriteSave.icl) and [`rewriteSave.dcl`](rewriteSave.dcl)):

    Contains the code used to save a rewrite to a file (`SaveRewrite`) and read a rewrite from a file (`ReadRewrite`).

-   **`utilities`** ([`utilities.icl`](utilities.icl) and [`utilities.dcl`](utilities.dcl)):

    This is sort of a catch-all module. It generally contains functions used by [`circuits.icl`](circuits.icl) (including code for drawing components and toolbox buttons), functions used by multiple other modules (including some code for doing variable substitutions), and general-purpose functions. See [`utilities.dcl`](utilities.dcl) for the full list.

-   **`debug`** ([`debug.icl`](debug.icl) and [`debug.dcl`](debug.dcl)):

    Contains code that is useful in debugging. Only `CheckConnections` is used in the program.
