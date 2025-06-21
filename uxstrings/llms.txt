================================================
FILE: readme.md
================================================
# Unicode Extended Strings (UXStrings)

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/uxstrings.json)](https://alire.ada.dev/crates/uxstrings.html)

## Motivation

My first motivation was to avoid the user of the Ada language from having to make a choice in the representation of character strings.
With the current Ada 2012 standard, the choice must be made according to the nature of the characters handled (Character, Wide\_Character or Wide\_Wide\_Character) and the adaptation of the string size according to the operations carried out.
Moreover, depending on the libraries used, making a single choice is generally not possible, which leads to continuous conversions.

Ada GUI library [Gnoga](https://sourceforge.net/projects/gnoga) internal character strings implementation is based on both Ada types String and Unbounded\_String.
The native Ada String encoding is Latin-1 whereas transactions with the Javascript part are in UTF-8 encoding.

Some drawbacks come up, for instance, with internationalization of programs (see [Localize Gnoga demo](https://sourceforge.net/p/gnoga/code/ci/dev_1.6/tree/demo/localize)):

* several conversions between String and Unbounded\_String objects
* it isn't usable out of Latin-1 character set, characters out of Latin-1 set are blanked
* continuous conversions between Latin-1 and UTF-8, each sent and received transaction between Ada and Javascript parts

Two ways of possible improvement for native Ada String: dynamic length handling and Unicode support.

## Workarounds

First possibility is using UTF-8 as internal implementation in Unbounded\_String objects.
The simplest way but Gnoga uses many times character indexes to parse Javascript messages that is not easy to achieved with UTF-8 which may have several lengths to represent one character. String parsing will be time consuming. Some combinations may lead to incorrect UTF-8 representation.

Second possibility is to use Unbounded\_Wide\_String or Unbounded\_Wide\_Wide\_String.
Using Unbounded\_Wide\_String is quite being in the middle of the river might as well use Unbounded\_Wide\_Wide\_String. In this latter case the memory penalty is heavy for only few accentuated character occurrences. So back to Unbounded\_Wide\_String but you'll miss the so essential emojis ;-)

Third possibility is to make no choice between Latin-1, Wide and Wide\_Wide characters. The object shall adapt its inner implementation to the actual content.
For instance with English language the most often use case will be Latin-1 inner implementation, for French language the most often will be Latin-1 with some exceptions with BMP (Unicode Basic Multilingual Plane) implementation such as in "cœur", for Greek language the most often will be BMP implementation.
The programmer won't make any representation choice when for example receiving UTF-8 messages:

``` ada
   S2 : UXString;
   ...
   S2 := "Received: " & From_UTF_8 (Message);
```

Automatically S2 will adapt its inner representation to the received characters.

## UXStrings packages

Package named [UXStrings](https://github.com/Blady-Com/UXStrings/blob/master/src/uxstrings1.ads) (Unicode Extended Strings) and its [Text_IO](https://github.com/Blady-Com/UXStrings/blob/master/src/uxstrings-text_io1.ads) child package are proposed to bring String enhancements using some Ada 2022 features.

The first part of UXStrings package contains renaming statements of current Ada types.
Ada current String type is structurally an array of Latin-1 characters thus is renamed as Latin\_1\_Character\_Array.
And so on.

The second part defines the USXString type as a tagged private type which has got aspects such as Constant\_Indexing, Variable\_Indexing, Iterable and String_Literal, so we can write:

``` ada
   S1, S2, S3 : UXString;
   C          : Character;
   WC         : Wide_Character;
   WWC        : Wide_Wide_Character;
   ...
   S1 := "était blah blah";
   C   := S1 (3);
   WC  := S1 (2);
   WWC := S1 (1);
   S1 (3) := WWC;
   S1 (2) := WC;
   S1 (1) := C;
   S3  := "une soirée passée à étudier les mathématiques ℕ⊂𝕂...";
   for I in S3 loop
      C   := S3 (I);
      WC  := S3 (I);
      WWC := S3 (I);
      Put_Line (Character'pos (C)'img & Wide_Character'pos (WC)'img & Wide_Wide_Character'pos (WWC)'img);
   end loop;
```

The third part defines conversion functions between UXString and various encoding such as Latin-1, BMP (USC-2), Unicode (USC-4), UTF-8 or UTF-16, so we can write:

``` ada
   S1  := From_Latin_1 ("blah blah");
   S2  := From_BMP ("une soirée passée à étudier la physique ω=Δθ/Δt...");
   S3  := From_Unicode ("une soirée passée à étudier les mathématiques ℕ⊂𝕂...");
   Send (To_UTF_8 (S1) & To_UTF_8 (S3));
```

The fourth part defines various API coming from Unbounded\_String such as Append, "&", Slice, "=", Index and so on.

Note: Iterable is a GNAT specific aspect.

With string lists we can write:

``` ada
   UXSL1 : constant UXStrings.Lists.UXString_List := ["Ada", "Strings", "Wide_Wide_Maps", "Wide_Wide_Constants", "Lower_Case_Map"];
...
   UXS1 : constant UXStrings.UXString := UXSL1.Join ('-').To_Lower;
```

## UXStrings implementations

### UXStrings 1

A first proof of concept implementation is provided. The source code files are ending with the number 1 as for instance "uxstrings1.ads". A GNAT project file "uxstrings.gpr" is provided with variant choice UXS_VARIANT set to UXS1 and some naming conventions for both packages UXStrings and UXStrings.Text\_IO.

#### Implementation choices

UTF-8 encoding is chosen for internal representation. The [Strings_Edit library](http://www.dmitry-kazakov.de/ada/strings_edit.htm) is used for UTF-8 encoding management.
[GNAT.OS_Lib](https://docs.adacore.com/gnat_rm-docs/html/gnat_rm/gnat_rm/the_gnat_library.html#gnat-os-lib-g-os-lib-ads) is chosen for input / output management.

### UXStrings 2

A second proof of concept implementation is provided. The source code files are ending with the number 2 as for instance "uxstrings2.ads". A GNAT project file "uxstrings.gpr" is provided with variant choice UXS_VARIANT set to UXS2 and some naming conventions for both packages UXStrings and UXStrings.Text\_IO.

#### Implementation choices

In addition to implementation UXStrings 1, some API have been added to support ASCII 7 bits encoding. ASCII is a subset of UTF-8 thus no change with the internal representation.
However, the API are now aware if content is full ASCII. On one hand, this permits to access directly to the position of one character without iterating on UTF-8 characters. Thus this is a time improvement when content is full ASCII. On the other hand, when content is changing the API check if the content is full ASCII. Thus this is a time penalty when changes are not full ASCII.

### UXStrings 3

A third proof of concept implementation is provided. The source code files are ending with the number 3 as for instance "uxstrings3.ads". A GNAT project file "uxstrings.gpr" is provided with variant choice UXS_VARIANT set to UXS3 and some naming conventions for both packages UXStrings and UXStrings.Text\_IO.

#### Implementation choices

In addition to implementation UXStrings 1, Unbounded\_Wide\_Wide\_Strings Ada standard package is chosen for internal representation. Characters are stored as Wide\_Wide\_Characters equivalent to Unicode. Memory management is done with the Unbounded capacity.

### UXStrings 4

A fourth proof of concept implementation is provided. The source code files are ending with the number 4 as for instance "uxstrings4.ads". A GNAT project file "uxstrings.gpr" is provided with variant choice UXS_VARIANT set to UXS4 and some naming conventions for both packages UXStrings and UXStrings.Text\_IO.

#### Implementation choices

In addition to implementation UXStrings 1, Ada.Containers.Vectors standard generic package is chosen for internal representation. Characters are stored as Wide\_Wide\_Characters equivalent to Unicode. Memory management is done with the container capacity.

### Children units

- UXStrings.Conversions: convenient subprograms to convert String into basic types and vice versa
- UXStrings.Formatting: subprogram formatting integers, floats, enumerates and more with format like lib C printf - ***NEW*** -
- UXStrings.Hash_case_insensitive: compute hash not case sensitive
- UXStrings.Hash: compute hash
- UXStrings.Lists: convenient subprograms to manage string lists
- UXStrings.Text_IO.Text_Streams: subprogram giving access to text streams
- UXStrings.Text_IO: standard Text_IO subprograms adapted to UXStrings

### Limitations

These implementations which are only for demonstrate the possible usages of UXString have many limitations.

- not thread safe
- single character assignment is not implemented in UXS1, UXS2 and UXS3 but is implemented in UXS4.
- only few API are implemented

### Future implementations

Here are some ideas:

- Use memory management as implemented in XStrings from [GNATColl](https://github.com/AdaCore/gnatcoll-core/blob/master/src/gnatcoll-strings_impl.ads).
- Adapt the inner implementation to the actual content with 8 bits character encodings, 16 bits or 32 bits.

## Tests

One test program [test\_uxstrings1.adb](https://github.com/Blady-Com/UXStrings/blob/master/tests/test_uxstrings1.adb) is provided for UXStrings tests and an other test program [test\_uxstrings\_text\_io.adb](https://github.com/Blady-Com/UXStrings/blob/master/tests/test_uxstrings_text_io.adb) is provided for UXStrings.Text\_IO tests.

## Dependencies

UXStrings library depends on [Strings Edit](http://www.dmitry-kazakov.de/ada/strings_edit.htm) library.
This latter is also part of [Simple Components](http://www.dmitry-kazakov.de/ada/components.htm) framework available on Alire.
Get one of these and add the path of strings\_edit.gpr in your GPR\_PROJECT_PATH before building your program with UXStrings.

## Using Alire

In your own [Alire](https://alire.ada.dev) project, add UXStrings dependency:

`% alr with uxstrings`

UXStrings with variant choice UXS_VARIANT set to UXS4 is default chosen implementation.

You might choose an other implementation 1, 2, 3 or 4 by setting variant choice UXS_VARIANT.

Then you can import the Ada UXStrings packages in your programs.

Note: Alire will take care of dependencies.

## Licence

The provided UXStrings specifications are intend to be public.
Constructive criticism and alternative implementations of these specifications are expected.
The actual proposed implementation is under [CeCILL-C](https://cecill.info) licence.

## Feedbacks

Feel free to send feedback about UXStrings specification source code on [Github](https://github.com/Blady-Com/UXStrings/issues).

Pascal Pignard, April 2021, August 2022, March-October 2023, April-October 2024.



================================================
FILE: src/uxstrings-conversions.ads
================================================
package UXStrings.Conversions is

   subtype Number_Base is Integer range 2 .. 16;
   -- Range of possible numerical base on integer values and images
   type Number_Prefix is (None, ' ', '+');
   -- List of possible prefix on integer images

   generic
      type T is (<>);
   function Scalar_Value (Item : UXString) return T;
   -- Return the conversion of the string Item into scalar value

   generic
      type T is range <>;
   function Integer_Value (Item : UXString; Base : in Number_Base := 10) return T;
   -- Return the conversion of the string Item into integer value with respect of specified Base

   generic
      type T is digits <>;
   function Floating_Point_Value (Item : UXString) return T;
   -- Return the conversion of the string Item into floating point value

   generic
      type T is delta <>;
   function Fixed_Point_Value (Item : UXString) return T;
   -- Return the conversion of the string Item into fixed point value

   generic
      type T is (<>);
   function Scalar_Image (Item : T) return UXString;
   -- Return the conversion of the scalar Item into string

   generic
      type T is range <>;
   function Integer_Image (Item : T; Base : in Number_Base := 10; Prefix : Number_Prefix := None) return UXString;
   -- Return the conversion of the integer Item into string with respect of specified Base and Prefix

   generic
      type T is digits <>;
   function Floating_Point_Image (Item : T) return UXString;
   -- Return the conversion of the floating point Item into string

   generic
      type T is delta <>;
   function Fixed_Point_Image (Item : T) return UXString;
   -- Return the conversion of the fixed point Item into string

end UXStrings.Conversions;



================================================
FILE: src/uxstrings-formatting.ads
================================================
with System;
with Ada.Strings;

private with GNAT.Formatted_String;

package UXStrings.Formatting is

   subtype Number_Base is Integer range 2 .. 16;
   -- Range of possible numerical base on integer values
   subtype Alignment is Ada.Strings.Alignment;
   -- List of possible alignment of integer values

   generic
      type T is range <>;
   function Integer_Format
     (Item    :    T; Base : in Number_Base := 10; Put_Plus : in Boolean := False; Field : in Natural := 0;
      Justify : in Alignment := Left; Fill : in Character := ' ') return UXString;
   -- Return the formatted string of the integer Item with respect of specified Base, Put_Plus sign,
   -- output Field size, alignment Justify, padding Fill

   type Formatted_UXString (<>) is private;
   -- See usage in GNAT.Formatted_String source file g-forstr.ads

   function Format (Spec : UXString) return Formatted_UXString;
   -- Create the formatted string with Spec format

   function From (Format : Formatted_UXString) return UXString;
   -- Return the formatted string with the respect of given parameters

   generic
      type T is (<>);
   function Formatted_Scalar (Left : Formatted_UXString; Right : T) return Formatted_UXString;
   -- Return the formatted form of the scalar item matching %c or %s

   function "&" (Left : Formatted_UXString; Right : Boolean) return Formatted_UXString;
   -- Return the formatted form of the Boolean item matching %s

   function "&" (Left : Formatted_UXString; Right : Character) return Formatted_UXString;
   -- Return the formatted form of the Character item matching %c

   generic
      type T is range <>;
   function Formatted_Integer (Left : Formatted_UXString; Right : T) return Formatted_UXString;
   -- Return the formatted form of the generic number item matching %d, %o, %x, %X

   function "&" (Left : Formatted_UXString; Right : Integer) return Formatted_UXString;
   -- Return the formatted form of the Integer item matching %d, %o, %x, %X

   generic
      type T is digits <>;
   function Formatted_Floating (Left : Formatted_UXString; Right : T) return Formatted_UXString;
   -- Return the formatted form of the generic floating item matching %f, %e, %F, %E, %g, %G

   function "&" (Left : Formatted_UXString; Right : Float) return Formatted_UXString;
   -- Return the formatted form of the Float item matching %f, %e, %F, %E, %g, %G

   generic
      type T is delta <>;
   function Formatted_Fixed_Point (Left : Formatted_UXString; Right : T) return Formatted_UXString;
   -- Return the formatted form of the generic fixed point item matching %f, %e, %F, %E, %g, %G

   function "&" (Left : Formatted_UXString; Right : Duration) return Formatted_UXString;
   -- Return the formatted form of the Duration item matching %f, %e, %F, %E, %g, %G

   function "&" (Left : Formatted_UXString; Right : System.Address) return Formatted_UXString;
      -- Return the formatted form of the Address item matching %p

   function "&" (Left : Formatted_UXString; Right : UXString) return Formatted_UXString;
   -- Return the formatted form of the UXString item matching %s

private

   type Formatted_UXString is new GNAT.Formatted_String.Formatted_String;

end UXStrings.Formatting;



================================================
FILE: src/uxstrings-hash.ads
================================================
with Ada.Containers;

function UXStrings.Hash (Key : UXString) return Ada.Containers.Hash_Type;
-- Return an implementation-defined value which is a function of the value of Key.
-- If A and B are strings such that A equals B then Hash(A) equals Hash(B).



================================================
FILE: src/uxstrings-hash_case_insensitive.ads
================================================
with Ada.Containers;

function UXStrings.Hash_Case_Insensitive (Key : UXString) return Ada.Containers.Hash_Type;
-- Returns an implementation-defined value which is a function of the value of Key, converted to lower case.
-- If A and B are strings such that Strings.Equal_Case_Insensitive (A, B) (see A.4.10) is True,
-- then Hash_Case_Insensitive(A) equals Hash_Case_Insensitive(B).



================================================
FILE: src/uxstrings-lists.ads
================================================
with Ada.Containers.Vectors;

package UXStrings.Lists is

   package UXString_Lists is new Ada.Containers.Vectors (Positive, UXString);
   type UXString_List is new UXString_Lists.Vector with private with
     Constant_Indexing => Constant_Reference, Variable_Indexing => Reference, Default_Iterator => Iterate,
     Iterator_Element  => UXString,
     Aggregate => (Empty => Empty, Add_Unnamed => Append, New_Indexed => New_Vector, Assign_Indexed => Replace_Element);

   function Constant_Reference
     (Container : aliased UXString_List; Index : Positive) return UXString_Lists.Constant_Reference_Type;
   function Reference (Container : aliased in out UXString_List; Index : Positive) return UXString_Lists.Reference_Type;

   procedure Append_Unique
     (Source : in out UXString_List; New_Item : UXString; Sensitivity : Case_Sensitivity := Sensitive);
   -- Update Source to the concatenation of Source and New_Item if New_Item is not already included with respect of case sensitivity

   function Filter
     (Source : UXString_List; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping := Identity)
      return UXString_List;
   -- Return a list of strings containing the pattern with respect of Mapping
   procedure Filter
     (Source : in out UXString_List; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping := Identity);
   -- Update Source with a list of strings containing the pattern with respect of Mapping
   function Filter
     (Source : UXString_List; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping_Function) return UXString_List;
   -- Return a list of strings containing the pattern with respect of Mapping
   procedure Filter (Source : in out UXString_List; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping_Function);
   -- Update Source with a list of strings containing the pattern with respect of Mapping
   function Filter
     (Source : UXString_List; Pattern : Wide_Wide_Character_Set; Test : Membership := Inside) return UXString_List;
   -- Return a list of strings containing the pattern character inside or outside Set matches Source with respect of Test membership
   procedure Filter (Source : in out UXString_List; Pattern : Wide_Wide_Character_Set; Test : Membership := Inside);
   -- Update Source with a list of strings containing the pattern character inside or outside Set matches Source with respect of Test membership

   function Join (Source : UXString_List; Separator : Unicode_Character) return UXString;
   -- Return a string with all the list's strings concatened and
   -- separated by the given separator
   function Join (Source : UXString_List; Separator : UXString) return UXString;
   -- Return a string with all the list's strings concatened and
   -- separated by the given separator (which can be an empty string)

   function Remove_Duplicates
     (Source : UXString_List; Sensitivity : Case_Sensitivity := Sensitive) return UXString_List;
   -- Return a list of strings without the duplicated ones with respect of case sensitivity
   procedure Remove_Duplicates
     (Source : in out UXString_List; Removed_Count : out Natural; Sensitivity : Case_Sensitivity := Sensitive);
   -- Update Source with the list strings without the duplicated ones and the number of removed entries
   -- with respect of sensitivity

   function Replace
     (Source : UXString_List; Before, After : UXString; Sensitivity : Case_Sensitivity := Sensitive)
      return UXString_List;
   -- Return a string list where every string has had the before text replaced with the after text
   -- wherever the before text is found with respect of case sensitivity
   procedure Replace
     (Source : in out UXString_List; Before, After : UXString; Sensitivity : Case_Sensitivity := Sensitive);
   -- Update Source where every string has had the before text replaced with the after text
   -- wherever the before text is found with respect of case sensitivity

   function Slice (Source : UXString_List; Low : Positive; High : Natural) return UXString_List;
   -- Return the slice at positions Low through High from Source

   function Sort (Source : UXString_List; Sensitivity : Case_Sensitivity := Sensitive) return UXString_List;
   -- Return a string list which is sorted in ascending order with respect of case sensitivity
   procedure Sort (Source : in out UXString_List; Sensitivity : Case_Sensitivity := Sensitive);
   -- Update Source with the sorted list in ascending order with respect of case sensitivity
   function Is_Sorted (Source : UXString_List; Sensitivity : Case_Sensitivity := Sensitive) return Boolean;
   -- Return True if Source is sorted with respect of case sensitivity
   function Merge (Left, Right : UXString_List; Sensitivity : Case_Sensitivity := Sensitive) return UXString_List;
   --- Return a string list with merging Left and Right string lists with respect of case sensitivity
   procedure Merge (Left : UXString_List; Right : in out UXString_List; Sensitivity : Case_Sensitivity := Sensitive);
   -- Update Source with the merged list of Left and Right string lists with respect of case sensitivity

private
   type UXString_List is new UXString_Lists.Vector with null record;
end UXStrings.Lists;



================================================
FILE: src/uxstrings-text_io-text_streams.ads
================================================
with Ada.Streams;
package UXStrings.Text_IO.Text_Streams is

   type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;

   function Stream (File : File_Type) return Stream_Access;
   -- Return an stream access to File

private

   type Stream_File is new Ada.Streams.Root_Stream_Type with record
      File : File_Access;
   end record;

   use Ada.Streams;

   overriding procedure Read
     (Stream : in out Stream_File; Item : out Stream_Element_Array; Last : out Stream_Element_Offset);

   overriding procedure Write (Stream : in out Stream_File; Item : Stream_Element_Array);

end UXStrings.Text_IO.Text_Streams;



================================================
FILE: src/uxstrings-text_io1.ads
================================================
with UXStrings.Lists;
private with GNAT.OS_Lib;

package UXStrings.Text_IO is

   type File_Type is limited private;
   type File_Access is access all File_Type;

   type File_Mode is (In_File, Out_File, Append_File);

   subtype Count is Natural range 0 .. Natural'Last;
   subtype Positive_Count is Count range 1 .. Count'Last;

   type Line_Ending is (CR_Ending, LF_Ending, CRLF_Ending);

   -- File Management

   procedure Create
     (File   : in out File_Type; Mode : in File_Mode := Out_File; Name : in UXString := Null_UXString;
      Scheme : in     Encoding_Scheme := Latin_1; Ending : Line_Ending := CRLF_Ending);
   procedure Open
     (File   : in out File_Type; Mode : in File_Mode; Name : in UXString; Scheme : in Encoding_Scheme := Latin_1;
      Ending :        Line_Ending := CRLF_Ending);

   procedure Close (File : in out File_Type);
   procedure Delete (File : in out File_Type);
   procedure Reset (File : in out File_Type; Mode : in File_Mode);
   procedure Reset (File : in out File_Type);

   function Mode (File : in File_Type) return File_Mode;
   function Name (File : in File_Type) return UXString;
   function Scheme (File : in File_Type) return Encoding_Scheme;
   procedure Scheme (File : in File_Access; Value : in Encoding_Scheme);
   function Ending (File : in File_Type) return Line_Ending;
   procedure Ending (File : in File_Access; Value : Line_Ending);

   function Is_Open (File : in File_Type) return Boolean;

   -- Control of default input and output files

   procedure Set_Input (File : in File_Type);
   procedure Set_Output (File : in File_Type);
   procedure Set_Error (File : in File_Type);

   function Standard_Input return File_Type;
   function Standard_Output return File_Type;
   function Standard_Error return File_Type;

   function Current_Input return File_Type;
   function Current_Output return File_Type;
   function Current_Error return File_Type;

   function Standard_Input return File_Access;
   function Standard_Output return File_Access;
   function Standard_Error return File_Access;

   function Current_Input return File_Access;
   function Current_Output return File_Access;
   function Current_Error return File_Access;

   -- Buffer control

   procedure Flush (File : in File_Type);
   procedure Flush;

   -- Specification of line and page lengths

   procedure Set_Line_Length (File : in File_Type; To : in Count);
   procedure Set_Line_Length (To : in Count);

   procedure Set_Page_Length (File : in File_Type; To : in Count);
   procedure Set_Page_Length (To : in Count);

   function Line_Length (File : in File_Type) return Count;
   function Line_Length return Count;

   function Page_Length (File : in File_Type) return Count;
   function Page_Length return Count;

   -- Column, Line, and Page Control

   procedure New_Line (File : in File_Type; Spacing : in Positive_Count := 1);
   procedure New_Line (Spacing : in Positive_Count := 1);

   procedure Skip_Line (File : in out File_Type; Spacing : in Positive_Count := 1);
   procedure Skip_Line (Spacing : in Positive_Count := 1);

   function End_Of_Line (File : in out File_Type) return Boolean;
   function End_Of_Line return Boolean;

   function Line_Mark return UXString;
   procedure Line_Mark (Ending : Line_Ending);

   procedure New_Page (File : in File_Type);
   procedure New_Page;

   procedure Skip_Page (File : in File_Type);
   procedure Skip_Page;

   function End_Of_Page (File : in File_Type) return Boolean;
   function End_Of_Page return Boolean;

   function Page_Mark return UXString;

   function End_Of_File (File : in out File_Type) return Boolean;
   function End_Of_File return Boolean;

   procedure Set_Col (File : in File_Type; To : in Positive_Count);
   procedure Set_Col (To : in Positive_Count);

   procedure Set_Line (File : in File_Type; To : in Positive_Count);
   procedure Set_Line (To : in Positive_Count);

   function Col (File : in File_Type) return Positive_Count;
   function Col return Positive_Count;

   function Line (File : in File_Type) return Positive_Count;
   function Line return Positive_Count;

   function Page (File : in File_Type) return Positive_Count;
   function Page return Positive_Count;

   -- Byte Order Mark Output (with respect of Encoding Scheme)

   procedure Put_BOM (File : in File_Type);
   procedure Put_BOM;

   -- Unicode Character Input-Output

   procedure Get (File : in out File_Type; Item : out Unicode_Character);
   procedure Get (Item : out Unicode_Character);

   procedure Put (File : in File_Type; Item : in Unicode_Character);
   procedure Put (Item : in Unicode_Character);

   procedure Look_Ahead (File : in out File_Type; Item : out Unicode_Character; End_Of_Line : out Boolean);
   procedure Look_Ahead (Item : out Unicode_Character; End_Of_Line : out Boolean);

   procedure Get_Immediate (File : in out File_Type; Item : out Unicode_Character);
   procedure Get_Immediate (Item : out Unicode_Character);

   procedure Get_Immediate (File : in out File_Type; Item : out Unicode_Character; Available : out Boolean);
   procedure Get_Immediate (Item : out Unicode_Character; Available : out Boolean);

   -- Unicode String Input-Output

   procedure Get (File : in out File_Type; Item : out UXString; Length : in Count);
   procedure Get (Item : out UXString; Length : in Count);

   procedure Put (File : in File_Type; Item : in UXString);
   procedure Put (Item : in UXString);

   procedure Get_Line (File : in out File_Type; Item : out UXString);
   procedure Get_Line (Item : out UXString);

   function Get_Line (File : in out File_Type) return UXString;
   function Get_Line return UXString;

   procedure Put_Line (File : in File_Type; Item : in UXString);
   procedure Put_Line (Item : in UXString);

   procedure Get_Text (File : in out File_Type; Item : out UXStrings.Lists.UXString_List; Count : Natural := 0);
   procedure Get_Text (Item : out UXStrings.Lists.UXString_List; Count : Natural := 0);

   function Get_Text (File : in out File_Type; Count : Natural := 0) return UXStrings.Lists.UXString_List;
   function Get_Text (Count : Natural := 0) return UXStrings.Lists.UXString_List;

   procedure Put_Text (File : in File_Type; Item : in UXStrings.Lists.UXString_List);
   procedure Put_Text (Item : in UXStrings.Lists.UXString_List);

   ----------------
   -- Exceptions --
   ----------------

   Status_Error : exception;
   Mode_Error   : exception;
   Name_Error   : exception;
   Use_Error    : exception;
   Device_Error : exception;
   End_Error    : exception;
   Data_Error   : exception;
   Layout_Error : exception;

private

   type String_Access is access String;
   type File_Type is record
      FD     : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Invalid_FD;
      Mode   : File_Mode;
      Name   : UXString;
      Scheme : Encoding_Scheme;
      Ending : Line_Ending;
      Buffer : String_Access               := new String'("");
      EOF    : Boolean                     := False;
   end record;

   procedure Read_Stream (File : in out File_Type; Item : out UTF_8_Character_Array; Last : out Natural);
   procedure Write_Stream (File : in out File_Type; Item : UTF_8_Character_Array);

end UXStrings.Text_IO;



================================================
FILE: src/uxstrings1.ads
================================================
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Wide_Wide_Maps; use Ada.Strings.Wide_Wide_Maps;
with Ada.Strings.UTF_Encoding;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Wide_Latin_1;
private with Ada.Finalization;
private with Ada.Streams;

package UXStrings is

   type Case_Sensitivity is (Insensitive, Sensitive);

   type Encoding_Scheme is (ASCII_7, Latin_1, UTF_8, UTF_16BE, UTF_16LE);
   -- Supported encoding schemes
   subtype UTF_16_Encoding_Scheme is Encoding_Scheme range UTF_16BE .. UTF_16LE;
   -- Supported UTF-16 encoding schemes

   subtype ASCII_Character is Ada.Characters.Handling.ISO_646;
   subtype ASCII_Character_Array is String with
       Dynamic_Predicate => (for all Item of ASCII_Character_Array => Item in ASCII_Character);
   -- Characters in ISO/IEC 646

   subtype Latin_1_Character is Character;
   subtype Latin_1_Character_Array is String;
   -- Characters in ISO/IEC 8859-1

   subtype BMP_Character is Wide_Character;
   subtype BMP_Character_Array is Wide_String;
   -- Characters in Unicode Basic Multilingual Plane
   -- (Could be also named UCS_2_Character (Universal Coded Character Set)?)

   subtype Unicode_Character is Wide_Wide_Character;
   subtype Unicode_Character_Array is Wide_Wide_String;
   -- Characters in Unicode planes
   -- (Could be also named UCS_4_Character?)

   subtype UTF_8_Character_Array is Ada.Strings.UTF_Encoding.UTF_String;
   subtype UTF_16_Character_Array is Ada.Strings.UTF_Encoding.UTF_String;
   -- Array of 8 bits values representing UTF encodings (UTF-8, UTF-16BE, or UTF-16LE)

   type UXString is tagged private with
     Constant_Indexing => Element,
     Iterable          => (First => First, Next => Next, Has_Element => Has_Element, Element => Element),
     String_Literal    => From_Unicode;
   -- Container type of Unicode characters with dynamic size usually named string

   Null_UXString : constant UXString;
   -- Represent the null string

   function Length (Source : UXString) return Natural;
   -- Return the number of (Unicode) characters

   function First (Source : UXString) return Positive;
   -- Return the position of the first character of Source (actually 1)
   function Next (Source : UXString; Index : Positive) return Positive;
   -- Return the position of the next character of Source after Index position (actually Index + 1)
   procedure Next (Source : UXString; Index : in out Positive);
   -- Update Index to the position of the next character of Source after Index position (actually Index + 1)
   function Has_Element (Source : UXString; Index : Positive) return Boolean;
   -- Return True if a character of Source is present at Index position (actually Index <= Length (Source))
   function Element (Source : UXString; Index : Positive) return Unicode_Character;
   -- Return the Unicode character of Source at Index position
   function Last (Source : UXString) return Natural;
   -- Return the position of the last character of Source (actually Length (Source))

   function Character_Set_Version return UXString;
   -- Returns an implementation-defined identifier that identifies the version of the character set standard
   -- that is used for categorizing characters by the implementation.

   Q_L : Latin_1_Character renames Ada.Characters.Latin_1.Question;

   function Is_ASCII (Source : UXString; Index : Positive) return Boolean;
   -- Return True if the character of Source at Index position is in ASCII set
   function Is_ASCII (Source : UXString) return Boolean;
   -- Return True if all the characters of Source are in ASCII set
   function Get_ASCII
     (Source : UXString; Index : Positive; Substitute : in ASCII_Character := Q_L) return ASCII_Character;
   -- Return the ASCII character of Source at Index position,
   -- if the character is not in ASCII set then Substitute is returned
   function To_ASCII (Source : UXString; Substitute : in ASCII_Character := Q_L) return ASCII_Character_Array;
   -- Return an array of ASCII characters from Source,
   -- if a character is not in ASCII set then Substitute is returned
   function From_ASCII (Item : ASCII_Character) return UXString;
   -- Return an UXString from the ASCII character Item
   function From_ASCII (Source : ASCII_Character_Array) return UXString;
   -- Return an UXString from the array of ASCII characters Source

   function Is_ISO_646 (Item : UXString) return Boolean renames Is_ASCII;
   function To_ISO_646
     (Item : UXString; Substitute : Ada.Characters.Handling.ISO_646 := Q_L) return ASCII_Character_Array renames
     To_ASCII;

   Inv_Q_L : Latin_1_Character renames Ada.Characters.Latin_1.Inverted_Question;

   function Is_Latin_1 (Source : UXString; Index : Positive) return Boolean;
   -- Return True if the character of Source at Index position is in Latin 1 set
   function Is_Latin_1 (Source : UXString) return Boolean;
   -- Return True if all the characters of Source are in Latin 1 set
   function Get_Latin_1
     (Source : UXString; Index : Positive; Substitute : in Latin_1_Character := Inv_Q_L) return Latin_1_Character;
   -- Return the Latin 1 character from Source at Index position,
   -- if the character is not in latin 1 set then Substitute is returned
   function To_Latin_1 (Source : UXString; Substitute : in Latin_1_Character := Inv_Q_L) return Latin_1_Character_Array;
   -- Return an array of Latin 1 characters from Source,
   -- if a character is not in latin 1 set then Substitute is returned
   function From_Latin_1 (Item : Latin_1_Character) return UXString;
   -- Return an UXString from the Latin 1 character parameter Item
   function From_Latin_1 (Source : Latin_1_Character_Array) return UXString;
   -- Return an UXString from the array of Latin 1 characters parameter Source

   Inv_Q_B : BMP_Character renames Ada.Characters.Wide_Latin_1.Inverted_Question;

   function Is_BMP (Source : UXString; Index : Positive) return Boolean;
   -- Return True if the character of Source at Index position is in BMP set
   function Is_BMP (Source : UXString) return Boolean;
   -- Return True if all the characters of Source are in BMP set
   function Get_BMP
     (Source : UXString; Index : Positive; Substitute : in BMP_Character := Inv_Q_B) return BMP_Character;
   -- Return the BMP character from Source at Index position,
   -- if the character is not in BMP set then Substitute is returned
   function To_BMP (Source : UXString; Substitute : in BMP_Character := Inv_Q_B) return BMP_Character_Array;
   -- Return an array of BMP characters from Source,
   -- if a character is not in BMP set then Substitute is returned
   function From_BMP (Item : BMP_Character) return UXString;
   -- Return an UXString from the BMP character parameter Item
   function From_BMP (Source : BMP_Character_Array) return UXString;
   -- Return an UXString from the array of BMP characters parameter Source

   function Is_Unicode (Source : UXString; Index : Positive) return Boolean;
   -- Return True if the character of Source at Index position is in Unicode set (actually True)
   function Is_Unicode (Source : UXString) return Boolean;
   -- Return True if all the characters of Source are in Unicode set (actually True)
   function Get_Unicode (Source : UXString; Index : Positive) return Unicode_Character;
   -- Return the Unicode character from Source at Index position
   function To_Unicode (Source : UXString) return Unicode_Character_Array;
   -- Return an array of Unicode characters from Source
   function From_Unicode (Item : Unicode_Character) return UXString;
   -- Return an UXString from the Unicode character parameter Item
   function From_Unicode (Source : Unicode_Character_Array) return UXString;
   -- Return an UXString from the array of Unicode characters parameter Source

   function To_UTF_8 (Source : UXString; Output_BOM : Boolean := False) return UTF_8_Character_Array;
   -- Return an array of UTF-8 characters from Source, prepend UTF-8 BOM if Output_BOM is set to True
   function From_UTF_8 (Source : UTF_8_Character_Array) return UXString;
   -- Return an UXString from the array of UTF-8 characters parameter Source,
   -- leading BOM characters are suppressed if any

   function To_UTF_16
     (Source : UXString; Output_Scheme : UTF_16_Encoding_Scheme; Output_BOM : Boolean := False)
      return UTF_16_Character_Array;
   -- Return an array of UTF-16 characters from Source according to the encoding scheme specified by Output_Scheme,
   -- prepend UTF-16 BOM if Output_BOM is set to True
   function From_UTF_16 (Source : UTF_16_Character_Array; Input_Scheme : UTF_16_Encoding_Scheme) return UXString;
   -- Return an UXString from the array of UTF-16 characters parameter Source
   -- according to the encoding scheme specified by Input_Scheme,
   -- leading BOM characters are suppressed if any

   procedure Set (Target : out UXString; Source : Unicode_Character_Array);
   -- Set Target to an UXString from the array of Unicode characters parameter Source

   procedure Append (Source : in out UXString; New_Item : UXString);
   -- Update Source to the concatenation of Source and New_Item
   procedure Append (Source : in out UXString; New_Item : Unicode_Character);
   -- Update Source to the concatenation of Source and New_Item

   procedure Prepend (Source : in out UXString; New_Item : UXString);
   -- Update Source to the concatenation of New_Item and Source
   procedure Prepend (Source : in out UXString; New_Item : Unicode_Character);
   -- Update Source to the concatenation of New_Item and Source

   function "&" (Left : UXString; Right : UXString) return UXString;
   -- Return the concatenation of Left and Right
   function "&" (Left : UXString; Right : Unicode_Character) return UXString;
   -- Return the concatenation of Left and Right
   function "&" (Left : Unicode_Character; Right : UXString) return UXString;
   -- Return the concatenation of Left and Right

   procedure Replace_ASCII (Source : in out UXString; Index : Positive; By : ASCII_Character);
   -- Update Source such as the character at Index position is set to the ASCII character parameter By
   procedure Replace_Latin_1 (Source : in out UXString; Index : Positive; By : Latin_1_Character);
   -- Update Source such as the character at Index position is set to the Latin 1 character parameter By
   procedure Replace_BMP (Source : in out UXString; Index : Positive; By : BMP_Character);
   -- Update Source such as the character at Index position is set to the BMP character parameter By
   procedure Replace_Unicode (Source : in out UXString; Index : Positive; By : Unicode_Character);
   -- Update Source such as the character at Index position is set to the Unicode character parameter By

   function Slice (Source : UXString; Low : Positive; High : Integer) return UXString;
   -- Return the slice at positions Low through High from Source
   procedure Slice (Source : UXString; Target : out UXString; Low : Positive; High : Integer);
   -- Set Target to the slice at positions Low through High from Source

   function "=" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left equals Right
   function "<" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left is less than Right
   function "<=" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left is less or equal than Right
   function ">" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left is greater than Right
   function ">=" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left greater or equal than Right

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : UXString; Pattern : UXString; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping := Identity) return Natural;
   -- Return the position of the first character where Pattern matches Source
   -- with respect of Going direction and Mapping
   function Index
     (Source  : UXString; Pattern : UXString; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping_Function) return Natural;
   -- Return the position of the first character where Pattern matches Source
   -- with respect of Going direction and Mapping
   function Index
     (Source : UXString; Set : Wide_Wide_Character_Set; Test : Membership := Inside; Going : Direction := Forward)
      return Natural;
   -- Return the position of the first character inside or outside Set matches Source
   -- with respect of Going direction and Test membership
   function Index
     (Source  : UXString; Pattern : UXString; From : Positive; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping := Identity) return Natural;
   -- Return the position of the first character where Pattern matches Source starting at From position
   -- with respect of Going direction and Mapping
   function Index
     (Source  : UXString; Pattern : UXString; From : Positive; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping_Function) return Natural;
   -- Return the position of the first character where Pattern matches Source starting at From position
   -- with respect of Going direction and Mapping
   function Index
     (Source : UXString; Set : Wide_Wide_Character_Set; From : Positive; Test : Membership := Inside;
      Going  : Direction := Forward) return Natural;
   -- Return the position of the first character inside or outside Set matches Source starting at From position
   -- with respect of Test membership

   function Index_Non_Blank (Source : UXString; Going : Direction := Forward) return Natural;
   -- Return the position of the first non space character of Source with respect of Going direction
   function Index_Non_Blank (Source : UXString; From : Positive; Going : Direction := Forward) return Natural;
   -- Return the position of the first non space character of Source starting at From position
   -- with respect of Going direction

   function Count
     (Source : UXString; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping := Identity) return Natural;
   -- Return the number of non overlapping occurrences of Pattern matching Source with respect of Mapping
   function Count
     (Source : UXString; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping_Function) return Natural;
   -- Return the number of non overlapping occurrences of Pattern matching Source with respect of Mapping
   function Count (Source : UXString; Set : Wide_Wide_Character_Set) return Natural;
   -- Return the number of occurrences of characters in parameter Set matching Source

   procedure Find_Token
     (Source :     UXString; Set : Wide_Wide_Character_Set; From : Positive; Test : Membership; First : out Positive;
      Last   : out Natural);
   -- Set First to position of the first character inside or outside Set matches Source starting at From position
   -- Set Last to position of the last character inside or outside Set matches Source with respect of Test membership
   procedure Find_Token
     (Source : UXString; Set : Wide_Wide_Character_Set; Test : Membership; First : out Positive; Last : out Natural);
   -- Set First to position of the first character inside or outside Set matches Source
   -- Set Last to position of the last character inside or outside Set matches Source with respect of Test membership

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate (Source : UXString; Mapping : Wide_Wide_Character_Mapping) return UXString;
   -- Return Source updated with respect of Mapping
   procedure Translate (Source : in out UXString; Mapping : Wide_Wide_Character_Mapping);
   -- Update Source with respect of Mapping
   function Translate (Source : UXString; Mapping : Wide_Wide_Character_Mapping_Function) return UXString;
   -- Return Source updated with respect of Mapping
   procedure Translate (Source : in out UXString; Mapping : Wide_Wide_Character_Mapping_Function);
   -- Update Source with respect of Mapping

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice (Source : UXString; Low : Positive; High : Natural; By : UXString) return UXString;
   -- Return Source whom characters with positions from Low to High are replaced with parameter By
   procedure Replace_Slice (Source : in out UXString; Low : Positive; High : Natural; By : UXString);
   -- Update Source whom characters with positions from Low to High are replaced with parameter By
   function Insert (Source : UXString; Before : Positive; New_Item : UXString) return UXString;
   -- Return Source with New_Item inserted at position ahead of parameter Before
   procedure Insert (Source : in out UXString; Before : Positive; New_Item : UXString);
   -- Update Source with New_Item inserted at position ahead of parameter Before
   function Overwrite (Source : UXString; Position : Positive; New_Item : UXString) return UXString;
   -- Return Source whom characters starting at Position are replaced with parameter New_Item
   procedure Overwrite (Source : in out UXString; Position : Positive; New_Item : UXString);
   -- Update Source whom characters starting at Position are replaced with parameter New_Item
   function Delete (Source : UXString; From : Positive; Through : Natural) return UXString;
   -- Return Source whom characters with positions from Low to High are removed
   procedure Delete (Source : in out UXString; From : Positive; Through : Natural);
   -- Update Source whom characters with positions from Low to High are removed
   function Trim (Source : UXString; Side : Trim_End) return UXString;
   -- Return Source with Space characters removed from left, right or both with respect of Side
   procedure Trim (Source : in out UXString; Side : Trim_End);
   -- Update Source with Space characters removed from left, right or both with respect of Side
   function Trim (Source : UXString; Left : Wide_Wide_Character_Set; Right : Wide_Wide_Character_Set) return UXString;
   -- Return Source with leading characters in Left and trailing characters in Right removed
   procedure Trim (Source : in out UXString; Left : Wide_Wide_Character_Set; Right : Wide_Wide_Character_Set);
   -- Update Source with leading characters in Left and trailing characters in Right removed
   function Head (Source : UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space) return UXString;
   -- Return the first characters from Source up to Count concatenated with Pad characters if needed
   procedure Head (Source : in out UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space);
   -- Update Source to the first characters from Source up to Count concatenated with Pad characters if needed
   function Tail (Source : UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space) return UXString;
   -- Return the last characters from Source up to Count concatenated with Pad characters if needed
   procedure Tail (Source : in out UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space);
   -- Update Source to the last characters from Source up to Count concatenated with Pad characters if needed

   function "*" (Left : Natural; Right : UXString) return UXString;
   -- Return Right string duplicated Left times
   function "*" (Left : Natural; Right : Unicode_Character) return UXString;
   -- Return Right character duplicated Left times

   ----------------------------------------------
   -- String Additional Comparison Subprograms --
   ----------------------------------------------

   function Equal_Case_Insensitive (Left, Right : UXString) return Boolean;
   -- Returns True if the strings consist of the same sequence of characters after applying locale-independent
   -- simple case folding, as defined by documents referenced in the note in Clause 1 of ISO/IEC 10646:2011.
   -- Otherwise, returns False.
   function Less_Case_Insensitive (Left, Right : UXString) return Boolean;
   -- Performs a lexicographic comparison of strings Left and Right, converted to lower case.

   -----------------------------------
   -- String Conversion Subprograms --
   -----------------------------------

   function To_Lower (Item : UXString) return UXString;
   -- Returns the corresponding lower-case value for Item if Is_Upper(Item), and returns Item otherwise.
   function To_Upper (Item : UXString) return UXString;
   -- Returns the corresponding upper-case value for Item if Is_Lower(Item) and Item has an upper-case form,
   -- and returns Item otherwise.
   function To_Basic (Item : UXString) return UXString;
   -- Returns the letter corresponding to Item but with no diacritical mark,
   -- if Item is a letter but not a basic letter; returns Item otherwise.

   -----------------------------------
   -- String additional Subprograms --
   -----------------------------------

   procedure Replace (Source : in out UXString; Before, After : UXString; Sensitivity : Case_Sensitivity := Sensitive);

private

   type UTF_8_Characters_Access is access UTF_8_Character_Array;
   type UXString is new Ada.Finalization.Controlled with record
      Chars     : UTF_8_Characters_Access := new UTF_8_Character_Array (2 .. 1);
      Finalized : Boolean                 := False;
   end record;

   procedure Adjust (Object : in out UXString);
   procedure Finalize (Object : in out UXString);

   procedure Bounded_Move (Source : in out UXString; Target : out UXString; Max : Natural; Last : out Natural);

   procedure UXString_Read (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out UXString);
   for UXString'Read use UXString_Read;

   procedure UXString_Write (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : UXString);
   for UXString'Write use UXString_Write;

   Null_UXString : constant UXString :=
     (Ada.Finalization.Controlled with Chars => new UTF_8_Character_Array (2 .. 1), Finalized => False);

end UXStrings;



================================================
FILE: src/uxstrings2.ads
================================================
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Wide_Wide_Maps; use Ada.Strings.Wide_Wide_Maps;
with Ada.Strings.UTF_Encoding;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Wide_Latin_1;
private with Ada.Finalization;
private with Ada.Streams;

package UXStrings is

   type Case_Sensitivity is (Insensitive, Sensitive);

   type Encoding_Scheme is (ASCII_7, Latin_1, UTF_8, UTF_16BE, UTF_16LE);
   -- Supported encoding schemes
   subtype UTF_16_Encoding_Scheme is Encoding_Scheme range UTF_16BE .. UTF_16LE;
   -- Supported UTF-16 encoding schemes

   subtype ASCII_Character is Ada.Characters.Handling.ISO_646;
   subtype ASCII_Character_Array is String with
       Dynamic_Predicate => (for all Item of ASCII_Character_Array => Item in ASCII_Character);
   -- Characters in ISO/IEC 646

   subtype Latin_1_Character is Character;
   subtype Latin_1_Character_Array is String;
   -- Characters in ISO/IEC 8859-1

   subtype BMP_Character is Wide_Character;
   subtype BMP_Character_Array is Wide_String;
   -- Characters in Unicode Basic Multilingual Plane
   -- (Could be also named UCS_2_Character (Universal Coded Character Set)?)

   subtype Unicode_Character is Wide_Wide_Character;
   subtype Unicode_Character_Array is Wide_Wide_String;
   -- Characters in Unicode planes
   -- (Could be also named UCS_4_Character?)

   subtype UTF_8_Character_Array is Ada.Strings.UTF_Encoding.UTF_String;
   subtype UTF_16_Character_Array is Ada.Strings.UTF_Encoding.UTF_String;
   -- Array of 8 bits values representing UTF encodings (UTF-8, UTF-16BE, or UTF-16LE)

   type UXString is tagged private with
     Constant_Indexing => Element,
     Iterable          => (First => First, Next => Next, Has_Element => Has_Element, Element => Element),
     String_Literal    => From_Unicode;
   -- Container type of Unicode characters with dynamic size usually named string

   Null_UXString : constant UXString;
   -- Represent the null string

   function Length (Source : UXString) return Natural;
   -- Return the number of (Unicode) characters

   function First (Source : UXString) return Positive;
   -- Return the position of the first character of Source (actually 1)
   function Next (Source : UXString; Index : Positive) return Positive;
   -- Return the position of the next character of Source after Index position (actually Index + 1)
   procedure Next (Source : UXString; Index : in out Positive);
   -- Update Index to the position of the next character of Source after Index position (actually Index + 1)
   function Has_Element (Source : UXString; Index : Positive) return Boolean;
   -- Return True if a character of Source is present at Index position (actually Index <= Length (Source))
   function Element (Source : UXString; Index : Positive) return Unicode_Character;
   -- Return the Unicode character of Source at Index position
   function Last (Source : UXString) return Natural;
   -- Return the position of the last character of Source (actually Length (Source))

   function Character_Set_Version return UXString;
   -- Returns an implementation-defined identifier that identifies the version of the character set standard
   -- that is used for categorizing characters by the implementation.

   Q_L : Latin_1_Character renames Ada.Characters.Latin_1.Question;

   function Is_ASCII (Source : UXString; Index : Positive) return Boolean;
   -- Return True if the character of Source at Index position is in ASCII set
   function Is_ASCII (Source : UXString) return Boolean;
   -- Return True if all the characters of Source are in ASCII set
   function Get_ASCII
     (Source : UXString; Index : Positive; Substitute : in ASCII_Character := Q_L) return ASCII_Character;
   -- Return the ASCII character of Source at Index position,
   -- if the character is not in ASCII set then Substitute is returned
   function To_ASCII (Source : UXString; Substitute : in ASCII_Character := Q_L) return ASCII_Character_Array;
   -- Return an array of ASCII characters from Source,
   -- if a character is not in ASCII set then Substitute is returned
   function From_ASCII (Item : ASCII_Character) return UXString;
   -- Return an UXString from the ASCII character Item
   function From_ASCII (Source : ASCII_Character_Array) return UXString;
   -- Return an UXString from the array of ASCII characters Source

   function Is_ISO_646 (Item : UXString) return Boolean renames Is_ASCII;
   function To_ISO_646
     (Item : UXString; Substitute : Ada.Characters.Handling.ISO_646 := Q_L) return ASCII_Character_Array renames
     To_ASCII;

   Inv_Q_L : Latin_1_Character renames Ada.Characters.Latin_1.Inverted_Question;

   function Is_Latin_1 (Source : UXString; Index : Positive) return Boolean;
   -- Return True if the character of Source at Index position is in Latin 1 set
   function Is_Latin_1 (Source : UXString) return Boolean;
   -- Return True if all the characters of Source are in Latin 1 set
   function Get_Latin_1
     (Source : UXString; Index : Positive; Substitute : in Latin_1_Character := Inv_Q_L) return Latin_1_Character;
   -- Return the Latin 1 character from Source at Index position,
   -- if the character is not in latin 1 set then Substitute is returned
   function To_Latin_1 (Source : UXString; Substitute : in Latin_1_Character := Inv_Q_L) return Latin_1_Character_Array;
   -- Return an array of Latin 1 characters from Source,
   -- if a character is not in latin 1 set then Substitute is returned
   function From_Latin_1 (Item : Latin_1_Character) return UXString;
   -- Return an UXString from the Latin 1 character parameter Item
   function From_Latin_1 (Source : Latin_1_Character_Array) return UXString;
   -- Return an UXString from the array of Latin 1 characters parameter Source

   Inv_Q_B : BMP_Character renames Ada.Characters.Wide_Latin_1.Inverted_Question;

   function Is_BMP (Source : UXString; Index : Positive) return Boolean;
   -- Return True if the character of Source at Index position is in BMP set
   function Is_BMP (Source : UXString) return Boolean;
   -- Return True if all the characters of Source are in BMP set
   function Get_BMP
     (Source : UXString; Index : Positive; Substitute : in BMP_Character := Inv_Q_B) return BMP_Character;
   -- Return the BMP character from Source at Index position,
   -- if the character is not in BMP set then Substitute is returned
   function To_BMP (Source : UXString; Substitute : in BMP_Character := Inv_Q_B) return BMP_Character_Array;
   -- Return an array of BMP characters from Source,
   -- if a character is not in BMP set then Substitute is returned
   function From_BMP (Item : BMP_Character) return UXString;
   -- Return an UXString from the BMP character parameter Item
   function From_BMP (Source : BMP_Character_Array) return UXString;
   -- Return an UXString from the array of BMP characters parameter Source

   function Is_Unicode (Source : UXString; Index : Positive) return Boolean;
   -- Return True if the character of Source at Index position is in Unicode set (actually True)
   function Is_Unicode (Source : UXString) return Boolean;
   -- Return True if all the characters of Source are in Unicode set (actually True)
   function Get_Unicode (Source : UXString; Index : Positive) return Unicode_Character;
   -- Return the Unicode character from Source at Index position
   function To_Unicode (Source : UXString) return Unicode_Character_Array;
   -- Return an array of Unicode characters from Source
   function From_Unicode (Item : Unicode_Character) return UXString;
   -- Return an UXString from the Unicode character parameter Item
   function From_Unicode (Source : Unicode_Character_Array) return UXString;
   -- Return an UXString from the array of Unicode characters parameter Source

   function To_UTF_8 (Source : UXString; Output_BOM : Boolean := False) return UTF_8_Character_Array;
   -- Return an array of UTF-8 characters from Source, prepend UTF-8 BOM if Output_BOM is set to True
   function From_UTF_8 (Source : UTF_8_Character_Array) return UXString;
   -- Return an UXString from the array of UTF-8 characters parameter Source,
   -- leading BOM characters are suppressed if any

   function To_UTF_16
     (Source : UXString; Output_Scheme : UTF_16_Encoding_Scheme; Output_BOM : Boolean := False)
      return UTF_16_Character_Array;
   -- Return an array of UTF-16 characters from Source according to the encoding scheme specified by Output_Scheme,
   -- prepend UTF-16 BOM if Output_BOM is set to True
   function From_UTF_16 (Source : UTF_16_Character_Array; Input_Scheme : UTF_16_Encoding_Scheme) return UXString;
   -- Return an UXString from the array of UTF-16 characters parameter Source
   -- according to the encoding scheme specified by Input_Scheme,
   -- leading BOM characters are suppressed if any

   procedure Set (Target : out UXString; Source : Unicode_Character_Array);
   -- Set Target to an UXString from the array of Unicode characters parameter Source

   procedure Append (Source : in out UXString; New_Item : UXString);
   -- Update Source to the concatenation of Source and New_Item
   procedure Append (Source : in out UXString; New_Item : Unicode_Character);
   -- Update Source to the concatenation of Source and New_Item

   procedure Prepend (Source : in out UXString; New_Item : UXString);
   -- Update Source to the concatenation of New_Item and Source
   procedure Prepend (Source : in out UXString; New_Item : Unicode_Character);
   -- Update Source to the concatenation of New_Item and Source

   function "&" (Left : UXString; Right : UXString) return UXString;
   -- Return the concatenation of Left and Right
   function "&" (Left : UXString; Right : Unicode_Character) return UXString;
   -- Return the concatenation of Left and Right
   function "&" (Left : Unicode_Character; Right : UXString) return UXString;
   -- Return the concatenation of Left and Right

   procedure Replace_ASCII (Source : in out UXString; Index : Positive; By : ASCII_Character);
   -- Update Source such as the character at Index position is set to the ASCII character parameter By
   procedure Replace_Latin_1 (Source : in out UXString; Index : Positive; By : Latin_1_Character);
   -- Update Source such as the character at Index position is set to the Latin 1 character parameter By
   procedure Replace_BMP (Source : in out UXString; Index : Positive; By : BMP_Character);
   -- Update Source such as the character at Index position is set to the BMP character parameter By
   procedure Replace_Unicode (Source : in out UXString; Index : Positive; By : Unicode_Character);
   -- Update Source such as the character at Index position is set to the Unicode character parameter By

   function Slice (Source : UXString; Low : Positive; High : Integer) return UXString;
   -- Return the slice at positions Low through High from Source
   procedure Slice (Source : UXString; Target : out UXString; Low : Positive; High : Integer);
   -- Set Target to the slice at positions Low through High from Source

   function "=" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left equals Right
   function "<" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left is less than Right
   function "<=" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left is less or equal than Right
   function ">" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left is greater than Right
   function ">=" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left greater or equal than Right

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : UXString; Pattern : UXString; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping := Identity) return Natural;
   -- Return the position of the first character where Pattern matches Source
   -- with respect of Going direction and Mapping
   function Index
     (Source  : UXString; Pattern : UXString; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping_Function) return Natural;
   -- Return the position of the first character where Pattern matches Source
   -- with respect of Going direction and Mapping
   function Index
     (Source : UXString; Set : Wide_Wide_Character_Set; Test : Membership := Inside; Going : Direction := Forward)
      return Natural;
   -- Return the position of the first character inside or outside Set matches Source
   -- with respect of Going direction and Test membership
   function Index
     (Source  : UXString; Pattern : UXString; From : Positive; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping := Identity) return Natural;
   -- Return the position of the first character where Pattern matches Source starting at From position
   -- with respect of Going direction and Mapping
   function Index
     (Source  : UXString; Pattern : UXString; From : Positive; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping_Function) return Natural;
   -- Return the position of the first character where Pattern matches Source starting at From position
   -- with respect of Going direction and Mapping
   function Index
     (Source : UXString; Set : Wide_Wide_Character_Set; From : Positive; Test : Membership := Inside;
      Going  : Direction := Forward) return Natural;
   -- Return the position of the first character inside or outside Set matches Source starting at From position
   -- with respect of Test membership

   function Index_Non_Blank (Source : UXString; Going : Direction := Forward) return Natural;
   -- Return the position of the first non space character of Source with respect of Going direction
   function Index_Non_Blank (Source : UXString; From : Positive; Going : Direction := Forward) return Natural;
   -- Return the position of the first non space character of Source starting at From position
   -- with respect of Going direction

   function Count
     (Source : UXString; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping := Identity) return Natural;
   -- Return the number of non overlapping occurrences of Pattern matching Source with respect of Mapping
   function Count
     (Source : UXString; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping_Function) return Natural;
   -- Return the number of non overlapping occurrences of Pattern matching Source with respect of Mapping
   function Count (Source : UXString; Set : Wide_Wide_Character_Set) return Natural;
   -- Return the number of occurrences of characters in parameter Set matching Source

   procedure Find_Token
     (Source :     UXString; Set : Wide_Wide_Character_Set; From : Positive; Test : Membership; First : out Positive;
      Last   : out Natural);
   -- Set First to position of the first character inside or outside Set matches Source starting at From position
   -- Set Last to position of the last character inside or outside Set matches Source with respect of Test membership
   procedure Find_Token
     (Source : UXString; Set : Wide_Wide_Character_Set; Test : Membership; First : out Positive; Last : out Natural);
   -- Set First to position of the first character inside or outside Set matches Source
   -- Set Last to position of the last character inside or outside Set matches Source with respect of Test membership

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate (Source : UXString; Mapping : Wide_Wide_Character_Mapping) return UXString;
   -- Return Source updated with respect of Mapping
   procedure Translate (Source : in out UXString; Mapping : Wide_Wide_Character_Mapping);
   -- Update Source with respect of Mapping
   function Translate (Source : UXString; Mapping : Wide_Wide_Character_Mapping_Function) return UXString;
   -- Return Source updated with respect of Mapping
   procedure Translate (Source : in out UXString; Mapping : Wide_Wide_Character_Mapping_Function);
   -- Update Source with respect of Mapping

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice (Source : UXString; Low : Positive; High : Natural; By : UXString) return UXString;
   -- Return Source whom characters with positions from Low to High are replaced with parameter By
   procedure Replace_Slice (Source : in out UXString; Low : Positive; High : Natural; By : UXString);
   -- Update Source whom characters with positions from Low to High are replaced with parameter By
   function Insert (Source : UXString; Before : Positive; New_Item : UXString) return UXString;
   -- Return Source with New_Item inserted at position ahead of parameter Before
   procedure Insert (Source : in out UXString; Before : Positive; New_Item : UXString);
   -- Update Source with New_Item inserted at position ahead of parameter Before
   function Overwrite (Source : UXString; Position : Positive; New_Item : UXString) return UXString;
   -- Return Source whom characters starting at Position are replaced with parameter New_Item
   procedure Overwrite (Source : in out UXString; Position : Positive; New_Item : UXString);
   -- Update Source whom characters starting at Position are replaced with parameter New_Item
   function Delete (Source : UXString; From : Positive; Through : Natural) return UXString;
   -- Return Source whom characters with positions from Low to High are removed
   procedure Delete (Source : in out UXString; From : Positive; Through : Natural);
   -- Update Source whom characters with positions from Low to High are removed
   function Trim (Source : UXString; Side : Trim_End) return UXString;
   -- Return Source with Space characters removed from left, right or both with respect of Side
   procedure Trim (Source : in out UXString; Side : Trim_End);
   -- Update Source with Space characters removed from left, right or both with respect of Side
   function Trim (Source : UXString; Left : Wide_Wide_Character_Set; Right : Wide_Wide_Character_Set) return UXString;
   -- Return Source with leading characters in Left and trailing characters in Right removed
   procedure Trim (Source : in out UXString; Left : Wide_Wide_Character_Set; Right : Wide_Wide_Character_Set);
   -- Update Source with leading characters in Left and trailing characters in Right removed
   function Head (Source : UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space) return UXString;
   -- Return the first characters from Source up to Count concatenated with Pad characters if needed
   procedure Head (Source : in out UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space);
   -- Update Source to the first characters from Source up to Count concatenated with Pad characters if needed
   function Tail (Source : UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space) return UXString;
   -- Return the last characters from Source up to Count concatenated with Pad characters if needed
   procedure Tail (Source : in out UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space);
   -- Update Source to the last characters from Source up to Count concatenated with Pad characters if needed

   function "*" (Left : Natural; Right : UXString) return UXString;
   -- Return Right string duplicated Left times
   function "*" (Left : Natural; Right : Unicode_Character) return UXString;
   -- Return Right character duplicated Left times

   ----------------------------------------------
   -- String Additional Comparison Subprograms --
   ----------------------------------------------

   function Equal_Case_Insensitive (Left, Right : UXString) return Boolean;
   -- Returns True if the strings consist of the same sequence of characters after applying locale-independent
   -- simple case folding, as defined by documents referenced in the note in Clause 1 of ISO/IEC 10646:2011.
   -- Otherwise, returns False.
   function Less_Case_Insensitive (Left, Right : UXString) return Boolean;
   -- Performs a lexicographic comparison of strings Left and Right, converted to lower case.

   -----------------------------------
   -- String Conversion Subprograms --
   -----------------------------------

   function To_Lower (Item : UXString) return UXString;
   -- Returns the corresponding lower-case value for Item if Is_Upper(Item), and returns Item otherwise.
   function To_Upper (Item : UXString) return UXString;
   -- Returns the corresponding upper-case value for Item if Is_Lower(Item) and Item has an upper-case form,
   -- and returns Item otherwise.
   function To_Basic (Item : UXString) return UXString;
   -- Returns the letter corresponding to Item but with no diacritical mark,
   -- if Item is a letter but not a basic letter; returns Item otherwise.

   -----------------------------------
   -- String additional Subprograms --
   -----------------------------------

   procedure Replace (Source : in out UXString; Before, After : UXString; Sensitivity : Case_Sensitivity := Sensitive);

private

   type UTF_8_Characters_Access is access UTF_8_Character_Array;
   type UXString is new Ada.Finalization.Controlled with record
      Chars      : UTF_8_Characters_Access := new UTF_8_Character_Array (2 .. 1);
      Full_ASCII : Boolean                 := False;
      Finalized  : Boolean                 := False;
   end record;

   procedure Adjust (Object : in out UXString);
   procedure Finalize (Object : in out UXString);

   procedure Bounded_Move (Source : in out UXString; Target : out UXString; Max : Natural; Last : out Natural);

   procedure UXString_Read (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out UXString);
   for UXString'Read use UXString_Read;

   procedure UXString_Write (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : UXString);
   for UXString'Write use UXString_Write;

   Null_UXString : constant UXString :=
     (Ada.Finalization.Controlled with Chars => new UTF_8_Character_Array (2 .. 1), Full_ASCII => True,
      Finalized                              => False);

end UXStrings;



================================================
FILE: src/uxstrings3.ads
================================================
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Wide_Wide_Maps; use Ada.Strings.Wide_Wide_Maps;
with Ada.Strings.UTF_Encoding;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Wide_Latin_1;
private with Ada.Strings.Wide_Wide_Unbounded;
private with Ada.Streams;

limited with UXStrings.Lists;

package UXStrings is

   type Case_Sensitivity is (Insensitive, Sensitive);

   type Encoding_Scheme is (ASCII_7, Latin_1, UTF_8, UTF_16BE, UTF_16LE);
   -- Supported encoding schemes
   subtype UTF_16_Encoding_Scheme is Encoding_Scheme range UTF_16BE .. UTF_16LE;
   -- Supported UTF-16 encoding schemes

   subtype ASCII_Character is Ada.Characters.Handling.ISO_646;
   subtype ASCII_Character_Array is String with
       Dynamic_Predicate => (for all Item of ASCII_Character_Array => Item in ASCII_Character);
   -- Characters in ISO/IEC 646

   subtype Latin_1_Character is Character;
   subtype Latin_1_Character_Array is String;
   -- Characters in ISO/IEC 8859-1

   subtype BMP_Character is Wide_Character;
   subtype BMP_Character_Array is Wide_String;
   -- Characters in Unicode Basic Multilingual Plane
   -- (Could be also named UCS_2_Character (Universal Coded Character Set)?)

   subtype Unicode_Character is Wide_Wide_Character;
   subtype Unicode_Character_Array is Wide_Wide_String;
   -- Characters in Unicode planes
   -- (Could be also named UCS_4_Character?)

   subtype UTF_8_Character_Array is Ada.Strings.UTF_Encoding.UTF_String;
   subtype UTF_16_Character_Array is Ada.Strings.UTF_Encoding.UTF_String;
   -- Array of 8 bits values representing UTF encodings (UTF-8, UTF-16BE, or UTF-16LE)

   type UXString is tagged private with
     Constant_Indexing => Element,
     Iterable          => (First => First, Next => Next, Has_Element => Has_Element, Element => Element),
     String_Literal    => From_Unicode;
   -- Container type of Unicode characters with dynamic size usually named string

   Null_UXString : constant UXString;
   -- Represent the null string

   function Length (Source : UXString) return Natural;
   -- Return the number of (Unicode) characters

   function First (Source : UXString) return Positive;
   -- Return the position of the first character of Source (actually 1)
   function Next (Source : UXString; Index : Positive) return Positive;
   -- Return the position of the next character of Source after Index position (actually Index + 1)
   procedure Next (Source : UXString; Index : in out Positive);
   -- Update Index to the position of the next character of Source after Index position (actually Index + 1)
   function Has_Element (Source : UXString; Index : Positive) return Boolean;
   -- Return True if a character of Source is present at Index position (actually Index <= Length (Source))
   function Element (Source : UXString; Index : Positive) return Unicode_Character;
   -- Return the Unicode character of Source at Index position
   function Last (Source : UXString) return Natural;
   -- Return the position of the last character of Source (actually Length (Source))

   function Character_Set_Version return UXString;
   -- Returns an implementation-defined identifier that identifies the version of the character set standard
   -- that is used for categorizing characters by the implementation.

   Q_L : Latin_1_Character renames Ada.Characters.Latin_1.Question;

   function Is_ASCII (Source : UXString; Index : Positive) return Boolean;
   -- Return True if the character of Source at Index position is in ASCII set
   function Is_ASCII (Source : UXString) return Boolean;
   -- Return True if all the characters of Source are in ASCII set
   function Get_ASCII
     (Source : UXString; Index : Positive; Substitute : in ASCII_Character := Q_L) return ASCII_Character;
   -- Return the ASCII character of Source at Index position,
   -- if the character is not in ASCII set then Substitute is returned
   function To_ASCII (Source : UXString; Substitute : in ASCII_Character := Q_L) return ASCII_Character_Array;
   -- Return an array of ASCII characters from Source,
   -- if a character is not in ASCII set then Substitute is returned
   function From_ASCII (Item : ASCII_Character) return UXString;
   -- Return an UXString from the ASCII character Item
   function From_ASCII (Source : ASCII_Character_Array) return UXString;
   -- Return an UXString from the array of ASCII characters Source

   function Is_ISO_646 (Item : UXString) return Boolean renames Is_ASCII;
   function To_ISO_646
     (Item : UXString; Substitute : Ada.Characters.Handling.ISO_646 := Q_L) return ASCII_Character_Array renames
     To_ASCII;

   Inv_Q_L : Latin_1_Character renames Ada.Characters.Latin_1.Inverted_Question;

   function Is_Latin_1 (Source : UXString; Index : Positive) return Boolean;
   -- Return True if the character of Source at Index position is in Latin 1 set
   function Is_Latin_1 (Source : UXString) return Boolean;
   -- Return True if all the characters of Source are in Latin 1 set
   function Get_Latin_1
     (Source : UXString; Index : Positive; Substitute : in Latin_1_Character := Inv_Q_L) return Latin_1_Character;
   -- Return the Latin 1 character from Source at Index position,
   -- if the character is not in latin 1 set then Substitute is returned
   function To_Latin_1 (Source : UXString; Substitute : in Latin_1_Character := Inv_Q_L) return Latin_1_Character_Array;
   -- Return an array of Latin 1 characters from Source,
   -- if a character is not in latin 1 set then Substitute is returned
   function From_Latin_1 (Item : Latin_1_Character) return UXString;
   -- Return an UXString from the Latin 1 character parameter Item
   function From_Latin_1 (Source : Latin_1_Character_Array) return UXString;
   -- Return an UXString from the array of Latin 1 characters parameter Source

   Inv_Q_B : BMP_Character renames Ada.Characters.Wide_Latin_1.Inverted_Question;

   function Is_BMP (Source : UXString; Index : Positive) return Boolean;
   -- Return True if the character of Source at Index position is in BMP set
   function Is_BMP (Source : UXString) return Boolean;
   -- Return True if all the characters of Source are in BMP set
   function Get_BMP
     (Source : UXString; Index : Positive; Substitute : in BMP_Character := Inv_Q_B) return BMP_Character;
   -- Return the BMP character from Source at Index position,
   -- if the character is not in BMP set then Substitute is returned
   function To_BMP (Source : UXString; Substitute : in BMP_Character := Inv_Q_B) return BMP_Character_Array;
   -- Return an array of BMP characters from Source,
   -- if a character is not in BMP set then Substitute is returned
   function From_BMP (Item : BMP_Character) return UXString;
   -- Return an UXString from the BMP character parameter Item
   function From_BMP (Source : BMP_Character_Array) return UXString;
   -- Return an UXString from the array of BMP characters parameter Source

   function Is_Unicode (Source : UXString; Index : Positive) return Boolean;
   -- Return True if the character of Source at Index position is in Unicode set (actually True)
   function Is_Unicode (Source : UXString) return Boolean;
   -- Return True if all the characters of Source are in Unicode set (actually True)
   function Get_Unicode (Source : UXString; Index : Positive) return Unicode_Character;
   -- Return the Unicode character from Source at Index position
   function To_Unicode (Source : UXString) return Unicode_Character_Array;
   -- Return an array of Unicode characters from Source
   function From_Unicode (Item : Unicode_Character) return UXString;
   -- Return an UXString from the Unicode character parameter Item
   function From_Unicode (Source : Unicode_Character_Array) return UXString;
   -- Return an UXString from the array of Unicode characters parameter Source

   function To_UTF_8 (Source : UXString; Output_BOM : Boolean := False) return UTF_8_Character_Array;
   -- Return an array of UTF-8 characters from Source, prepend UTF-8 BOM if Output_BOM is set to True
   function From_UTF_8 (Source : UTF_8_Character_Array) return UXString;
   -- Return an UXString from the array of UTF-8 characters parameter Source,
   -- leading BOM characters are suppressed if any

   function To_UTF_16
     (Source : UXString; Output_Scheme : UTF_16_Encoding_Scheme; Output_BOM : Boolean := False)
      return UTF_16_Character_Array;
   -- Return an array of UTF-16 characters from Source according to the encoding scheme specified by Output_Scheme,
   -- prepend UTF-16 BOM if Output_BOM is set to True
   function From_UTF_16 (Source : UTF_16_Character_Array; Input_Scheme : UTF_16_Encoding_Scheme) return UXString;
   -- Return an UXString from the array of UTF-16 characters parameter Source
   -- according to the encoding scheme specified by Input_Scheme,
   -- leading BOM characters are suppressed if any

   procedure Set (Target : out UXString; Source : Unicode_Character_Array);
   -- Set Target to an UXString from the array of Unicode characters parameter Source

   procedure Append (Source : in out UXString; New_Item : UXString);
   -- Update Source to the concatenation of Source and New_Item
   procedure Append (Source : in out UXString; New_Item : Unicode_Character);
   -- Update Source to the concatenation of Source and New_Item

   procedure Prepend (Source : in out UXString; New_Item : UXString);
   -- Update Source to the concatenation of New_Item and Source
   procedure Prepend (Source : in out UXString; New_Item : Unicode_Character);
   -- Update Source to the concatenation of New_Item and Source

   function "&" (Left : UXString; Right : UXString) return UXString;
   -- Return the concatenation of Left and Right
   function "&" (Left : UXString; Right : Unicode_Character) return UXString;
   -- Return the concatenation of Left and Right
   function "&" (Left : Unicode_Character; Right : UXString) return UXString;
   -- Return the concatenation of Left and Right

   procedure Replace_ASCII (Source : in out UXString; Index : Positive; By : ASCII_Character);
   -- Update Source such as the character at Index position is set to the ASCII character parameter By
   procedure Replace_Latin_1 (Source : in out UXString; Index : Positive; By : Latin_1_Character);
   -- Update Source such as the character at Index position is set to the Latin 1 character parameter By
   procedure Replace_BMP (Source : in out UXString; Index : Positive; By : BMP_Character);
   -- Update Source such as the character at Index position is set to the BMP character parameter By
   procedure Replace_Unicode (Source : in out UXString; Index : Positive; By : Unicode_Character);
   -- Update Source such as the character at Index position is set to the Unicode character parameter By

   function Slice (Source : UXString; Low : Positive; High : Integer) return UXString;
   -- Return the slice at positions Low through High from Source
   procedure Slice (Source : UXString; Target : out UXString; Low : Integer; High : Natural);
   -- Set Target to the slice at positions Low through High from Source

   function "=" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left equals Right
   function "<" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left is less than Right
   function "<=" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left is less or equal than Right
   function ">" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left is greater than Right
   function ">=" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left greater or equal than Right

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : UXString; Pattern : UXString; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping := Identity) return Natural;
   -- Return the position of the first character where Pattern matches Source
   -- with respect of Going direction and Mapping
   function Index
     (Source  : UXString; Pattern : UXString; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping_Function) return Natural;
   -- Return the position of the first character where Pattern matches Source
   -- with respect of Going direction and Mapping
   function Index
     (Source : UXString; Set : Wide_Wide_Character_Set; Test : Membership := Inside; Going : Direction := Forward)
      return Natural;
   -- Return the position of the first character inside or outside Set matches Source
   -- with respect of Going direction and Test membership
   function Index
     (Source  : UXString; Pattern : UXString; From : Positive; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping := Identity) return Natural;
   -- Return the position of the first character where Pattern matches Source starting at From position
   -- with respect of Going direction and Mapping
   function Index
     (Source  : UXString; Pattern : UXString; From : Positive; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping_Function) return Natural;
   -- Return the position of the first character where Pattern matches Source starting at From position
   -- with respect of Going direction and Mapping
   function Index
     (Source : UXString; Set : Wide_Wide_Character_Set; From : Positive; Test : Membership := Inside;
      Going  : Direction := Forward) return Natural;
   -- Return the position of the first character inside or outside Set matches Source starting at From position
   -- with respect of Test membership

   function Index_Non_Blank (Source : UXString; Going : Direction := Forward) return Natural;
   -- Return the position of the first non space character of Source with respect of Going direction
   function Index_Non_Blank (Source : UXString; From : Positive; Going : Direction := Forward) return Natural;
   -- Return the position of the first non space character of Source starting at From position
   -- with respect of Going direction

   function Count
     (Source : UXString; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping := Identity) return Natural;
   -- Return the number of non overlapping occurrences of Pattern matching Source with respect of Mapping
   function Count
     (Source : UXString; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping_Function) return Natural;
   -- Return the number of non overlapping occurrences of Pattern matching Source with respect of Mapping
   function Count (Source : UXString; Set : Wide_Wide_Character_Set) return Natural;
   -- Return the number of occurrences of characters in parameter Set matching Source

   procedure Find_Token
     (Source :     UXString; Set : Wide_Wide_Character_Set; From : Positive; Test : Membership; First : out Positive;
      Last   : out Natural);
   -- Set First to position of the first character inside or outside Set matches Source starting at From position
   -- Set Last to position of the last character inside or outside Set matches Source with respect of Test membership
   procedure Find_Token
     (Source : UXString; Set : Wide_Wide_Character_Set; Test : Membership; First : out Positive; Last : out Natural);
   -- Set First to position of the first character inside or outside Set matches Source
   -- Set Last to position of the last character inside or outside Set matches Source with respect of Test membership

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate (Source : UXString; Mapping : Wide_Wide_Character_Mapping) return UXString;
   -- Return Source updated with respect of Mapping
   procedure Translate (Source : in out UXString; Mapping : Wide_Wide_Character_Mapping);
   -- Update Source with respect of Mapping
   function Translate (Source : UXString; Mapping : Wide_Wide_Character_Mapping_Function) return UXString;
   -- Return Source updated with respect of Mapping
   procedure Translate (Source : in out UXString; Mapping : Wide_Wide_Character_Mapping_Function);
   -- Update Source with respect of Mapping

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice (Source : UXString; Low : Positive; High : Natural; By : UXString) return UXString;
   -- Return Source whom characters with positions from Low to High are replaced with parameter By
   procedure Replace_Slice (Source : in out UXString; Low : Positive; High : Natural; By : UXString);
   -- Update Source whom characters with positions from Low to High are replaced with parameter By
   function Insert (Source : UXString; Before : Positive; New_Item : UXString) return UXString;
   -- Return Source with New_Item inserted at position ahead of parameter Before
   procedure Insert (Source : in out UXString; Before : Positive; New_Item : UXString);
   -- Update Source with New_Item inserted at position ahead of parameter Before
   function Overwrite (Source : UXString; Position : Positive; New_Item : UXString) return UXString;
   -- Return Source whom characters starting at Position are replaced with parameter New_Item
   procedure Overwrite (Source : in out UXString; Position : Positive; New_Item : UXString);
   -- Update Source whom characters starting at Position are replaced with parameter New_Item
   function Delete (Source : UXString; From : Positive; Through : Natural) return UXString;
   -- Return Source whom characters with positions from Low to High are removed
   procedure Delete (Source : in out UXString; From : Positive; Through : Natural);
   -- Update Source whom characters with positions from Low to High are removed
   function Trim (Source : UXString; Side : Trim_End) return UXString;
   -- Return Source with Space characters removed from left, right or both with respect of Side
   procedure Trim (Source : in out UXString; Side : Trim_End);
   -- Update Source with Space characters removed from left, right or both with respect of Side
   function Trim (Source : UXString; Left : Wide_Wide_Character_Set; Right : Wide_Wide_Character_Set) return UXString;
   -- Return Source with leading characters in Left and trailing characters in Right removed
   procedure Trim (Source : in out UXString; Left : Wide_Wide_Character_Set; Right : Wide_Wide_Character_Set);
   -- Update Source with leading characters in Left and trailing characters in Right removed
   function Head (Source : UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space) return UXString;
   -- Return the first characters from Source up to Count concatenated with Pad characters if needed
   procedure Head (Source : in out UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space);
   -- Update Source to the first characters from Source up to Count concatenated with Pad characters if needed
   function Tail (Source : UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space) return UXString;
   -- Return the last characters from Source up to Count concatenated with Pad characters if needed
   procedure Tail (Source : in out UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space);
   -- Update Source to the last characters from Source up to Count concatenated with Pad characters if needed

   function "*" (Left : Natural; Right : UXString) return UXString;
   -- Return Right string duplicated Left times
   function "*" (Left : Natural; Right : Unicode_Character) return UXString;
   -- Return Right character duplicated Left times

   ----------------------------------------------
   -- String Additional Comparison Subprograms --
   ----------------------------------------------

   function Equal_Case_Insensitive (Left, Right : UXString) return Boolean;
   -- Returns True if the strings consist of the same sequence of characters after applying locale-independent
   -- simple case folding, as defined by documents referenced in the note in Clause 1 of ISO/IEC 10646:2011.
   -- Otherwise, returns False.
   function Less_Case_Insensitive (Left, Right : UXString) return Boolean;
   -- Performs a lexicographic comparison of strings Left and Right, converted to lower case.

   -----------------------------------
   -- String Conversion Subprograms --
   -----------------------------------

   function To_Lower (Item : UXString) return UXString;
   -- Returns the corresponding lower-case value for Item if Is_Upper(Item), and returns Item otherwise.
   function To_Upper (Item : UXString) return UXString;
   -- Returns the corresponding upper-case value for Item if Is_Lower(Item) and Item has an upper-case form,
   -- and returns Item otherwise.
   function To_Basic (Item : UXString) return UXString;
   -- Returns the letter corresponding to Item but with no diacritical mark,
   -- if Item is a letter but not a basic letter; returns Item otherwise.

   -----------------------------------
   -- String additional Subprograms --
   -----------------------------------

   function Contains
     (Source : UXString; Pattern : UXString; Sensitivity : Case_Sensitivity := Sensitive) return Boolean;
   -- Return True if Source contains Pattern with respect case of sensitivity

   function Ends_With
     (Source : UXString; Pattern : UXString; Sensitivity : Case_Sensitivity := Sensitive) return Boolean;
   -- Return True if Source ends with pattern with respect case of sensitivity

   function Starts_With
     (Source : UXString; Pattern : UXString; Sensitivity : Case_Sensitivity := Sensitive) return Boolean;
   -- Return True if Source starts with Pattern with respect case of sensitivity

   function Is_Lower (Source : UXString) return Boolean;
   -- Return True if Source is lowercase

   function Is_Upper (Source : UXString) return Boolean;
   -- Return True if Source is uppercase

   function Is_Basic (Source : UXString) return Boolean;
   -- Return True if source is basic (with no diacritical mark)

   function Is_Empty (Source : UXString) return Boolean;
   -- Return True is Source is empty (equal to Null_UXString)

   function Remove
     (Source : UXString; Pattern : Unicode_Character; Sensitivity : Case_Sensitivity := Sensitive) return UXString;
   -- Return Source where every occurrence of Pattern have been removed with respect of case sensitivity
   procedure Remove
     (Source : in out UXString; Pattern : Unicode_Character; Sensitivity : Case_Sensitivity := Sensitive);
   -- Update Source where every occurrence of Pattern have been removed with respect of case sensitivity

   function Remove (Source : UXString; Pattern : UXString; Sensitivity : Case_Sensitivity := Sensitive) return UXString;
   -- Return Source where every occurrence of Pattern have been removed with respect of case sensitivity
   procedure Remove (Source : in out UXString; Pattern : UXString; Sensitivity : Case_Sensitivity := Sensitive);
   -- Update Source where every occurrence of Pattern have been removed with respect of case sensitivity

   function Replace
     (Source : UXString; Before, After : Unicode_Character; Sensitivity : Case_Sensitivity := Sensitive)
      return UXString;
   -- Return a string which has had the before character replaced with the after character
   -- wherever the before character is found with respect of sensitivity
   procedure Replace
     (Source : in out UXString; Before, After : Unicode_Character; Sensitivity : Case_Sensitivity := Sensitive);
   -- Update Source which has had the before character replaced with the after character
   -- wherever the before character is found with respect of sensitivity
   function Replace
     (Source : UXString; Before, After : UXString; Sensitivity : Case_Sensitivity := Sensitive) return UXString;
   -- Return a string which has had the before text replaced with the after text
   -- wherever the before text is found with respect of sensitivity
   procedure Replace (Source : in out UXString; Before, After : UXString; Sensitivity : Case_Sensitivity := Sensitive);
   -- Update Source which has had the before text replaced with the after text
   -- wherever the before text is found with respect of case sensitivity

   ------------------------------
   -- String split Subprograms --
   ------------------------------

   function Split
     (Source           : UXString; Separator : Unicode_Character; Sensitivity : Case_Sensitivity := Sensitive;
      Keep_Empty_Parts : Boolean := True) return UXStrings.Lists.UXString_List;
   -- Return a string list resulting in spliting Source into substrings wherever Separator occurs
   function Split
     (Source           : UXString; Separator : UXString; Sensitivity : Case_Sensitivity := Sensitive;
      Keep_Empty_Parts : Boolean := True) return UXStrings.Lists.UXString_List;
   -- Return a string list resulting in spliting Source into substrings wherever Separator occurs
   function Split
     (Source           : UXString; Separator : Wide_Wide_Character_Set; Test : Membership := Inside;
      Keep_Empty_Parts : Boolean := True) return UXStrings.Lists.UXString_List;
   -- Return a string list resulting in spliting Source into substrings wherever Separator occurs with respect of Test membership

private

   type UTF_8_Characters_Access is access UTF_8_Character_Array;
   type UXString is tagged record
      Chars : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
   end record;

   procedure Bounded_Move (Source : in out UXString; Target : out UXString; Max : Natural; Last : out Natural);

   procedure UXString_Read (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out UXString);
   for UXString'Read use UXString_Read;

   procedure UXString_Write (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : UXString);
   for UXString'Write use UXString_Write;

   Null_UXString : constant UXString := (Chars => Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String);

end UXStrings;



================================================
FILE: src/uxstrings4.ads
================================================
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Wide_Wide_Maps; use Ada.Strings.Wide_Wide_Maps;
with Ada.Strings.UTF_Encoding;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Wide_Latin_1;
with Ada.Containers.Vectors;
private with Ada.Streams;

limited with UXStrings.Lists;

package UXStrings is

   type Case_Sensitivity is (Insensitive, Sensitive);

   type Encoding_Scheme is (ASCII_7, Latin_1, UTF_8, UTF_16BE, UTF_16LE);
   -- Supported encoding schemes
   subtype UTF_16_Encoding_Scheme is Encoding_Scheme range UTF_16BE .. UTF_16LE;
   -- Supported UTF-16 encoding schemes

   subtype ASCII_Character is Ada.Characters.Handling.ISO_646;
   subtype ASCII_Character_Array is String with
       Dynamic_Predicate => (for all Item of ASCII_Character_Array => Item in ASCII_Character);
   -- Characters in ISO/IEC 646

   subtype Latin_1_Character is Character;
   subtype Latin_1_Character_Array is String;
   -- Characters in ISO/IEC 8859-1

   subtype BMP_Character is Wide_Character;
   subtype BMP_Character_Array is Wide_String;
   -- Characters in Unicode Basic Multilingual Plane
   -- (Could be also named UCS_2_Character (Universal Coded Character Set)?)

   subtype Unicode_Character is Wide_Wide_Character;
   subtype Unicode_Character_Array is Wide_Wide_String;
   -- Characters in Unicode planes
   -- (Could be also named UCS_4_Character?)

   subtype UTF_8_Character_Array is Ada.Strings.UTF_Encoding.UTF_String;
   subtype UTF_16_Character_Array is Ada.Strings.UTF_Encoding.UTF_String;
   -- Array of 8 bits values representing UTF encodings (UTF-8, UTF-16BE, or UTF-16LE)

   package UXString_Vector is new Ada.Containers.Vectors (Positive, Unicode_Character);
   type UXString is tagged private with
     Constant_Indexing => Constant_Reference, Variable_Indexing => Reference, Default_Iterator => Iterate,
     Iterator_Element  => Unicode_Character,
     Aggregate => (Empty => Empty, Add_Unnamed => Append, New_Indexed => New_Vector, Assign_Indexed => Replace_Element),
     String_Literal    => From_Unicode;
   -- Container type of Unicode characters with dynamic size usually named string

   function Constant_Reference
     (Container : aliased UXString; Index : Positive) return UXString_Vector.Constant_Reference_Type;
   function Reference (Container : aliased in out UXString; Index : Positive) return UXString_Vector.Reference_Type;
   function Iterate (Container : UXString) return UXString_Vector.Vector_Iterator_Interfaces.Reversible_Iterator'Class;
   function Iterate
     (Container : UXString; Start : UXString_Vector.Cursor)
      return UXString_Vector.Vector_Iterator_Interfaces.Reversible_Iterator'Class;
   function Empty (Capacity : Natural := 10) return UXString;
   function New_Vector (First, Last : Positive) return UXString;
   procedure Replace_Element (Container : in out UXString; Index : Positive; New_Item : Unicode_Character);

   Null_UXString : constant UXString;
   -- Represent the null string

   function Length (Source : UXString) return Natural;
   -- Return the number of (Unicode) characters

   function First (Source : UXString) return Positive;
   -- Return the position of the first character of Source (actually 1)
   function Next (Source : UXString; Index : Positive) return Positive;
   -- Return the position of the next character of Source after Index position (actually Index + 1)
   procedure Next (Source : UXString; Index : in out Positive);
   -- Update Index to the position of the next character of Source after Index position (actually Index + 1)
   function Has_Element (Source : UXString; Index : Positive) return Boolean;
   -- Return True if a character of Source is present at Index position (actually Index <= Length (Source))
   function Element (Source : UXString; Index : Positive) return Unicode_Character;
   -- Return the Unicode character of Source at Index position
   function Last (Source : UXString) return Natural;
   -- Return the position of the last character of Source (actually Length (Source))

   function Character_Set_Version return UXString;
   -- Returns an implementation-defined identifier that identifies the version of the character set standard
   -- that is used for categorizing characters by the implementation.

   Q_L : Latin_1_Character renames Ada.Characters.Latin_1.Question;

   function Is_ASCII (Source : UXString; Index : Positive) return Boolean;
   -- Return True if the character of Source at Index position is in ASCII set
   function Is_ASCII (Source : UXString) return Boolean;
   -- Return True if all the characters of Source are in ASCII set
   function Get_ASCII
     (Source : UXString; Index : Positive; Substitute : in ASCII_Character := Q_L) return ASCII_Character;
   -- Return the ASCII character of Source at Index position,
   -- if the character is not in ASCII set then Substitute is returned
   function To_ASCII (Source : UXString; Substitute : in ASCII_Character := Q_L) return ASCII_Character_Array;
   -- Return an array of ASCII characters from Source,
   -- if a character is not in ASCII set then Substitute is returned
   function From_ASCII (Item : ASCII_Character) return UXString;
   -- Return an UXString from the ASCII character Item
   function From_ASCII (Source : ASCII_Character_Array) return UXString;
   -- Return an UXString from the array of ASCII characters Source

   function Is_ISO_646 (Item : UXString) return Boolean renames Is_ASCII;
   function To_ISO_646
     (Item : UXString; Substitute : Ada.Characters.Handling.ISO_646 := Q_L) return ASCII_Character_Array renames
     To_ASCII;

   Inv_Q_L : Latin_1_Character renames Ada.Characters.Latin_1.Inverted_Question;

   function Is_Latin_1 (Source : UXString; Index : Positive) return Boolean;
   -- Return True if the character of Source at Index position is in Latin 1 set
   function Is_Latin_1 (Source : UXString) return Boolean;
   -- Return True if all the characters of Source are in Latin 1 set
   function Get_Latin_1
     (Source : UXString; Index : Positive; Substitute : in Latin_1_Character := Inv_Q_L) return Latin_1_Character;
   -- Return the Latin 1 character from Source at Index position,
   -- if the character is not in latin 1 set then Substitute is returned
   function To_Latin_1 (Source : UXString; Substitute : in Latin_1_Character := Inv_Q_L) return Latin_1_Character_Array;
   -- Return an array of Latin 1 characters from Source,
   -- if a character is not in latin 1 set then Substitute is returned
   function From_Latin_1 (Item : Latin_1_Character) return UXString;
   -- Return an UXString from the Latin 1 character parameter Item
   function From_Latin_1 (Source : Latin_1_Character_Array) return UXString;
   -- Return an UXString from the array of Latin 1 characters parameter Source

   Inv_Q_B : BMP_Character renames Ada.Characters.Wide_Latin_1.Inverted_Question;

   function Is_BMP (Source : UXString; Index : Positive) return Boolean;
   -- Return True if the character of Source at Index position is in BMP set
   function Is_BMP (Source : UXString) return Boolean;
   -- Return True if all the characters of Source are in BMP set
   function Get_BMP
     (Source : UXString; Index : Positive; Substitute : in BMP_Character := Inv_Q_B) return BMP_Character;
   -- Return the BMP character from Source at Index position,
   -- if the character is not in BMP set then Substitute is returned
   function To_BMP (Source : UXString; Substitute : in BMP_Character := Inv_Q_B) return BMP_Character_Array;
   -- Return an array of BMP characters from Source,
   -- if a character is not in BMP set then Substitute is returned
   function From_BMP (Item : BMP_Character) return UXString;
   -- Return an UXString from the BMP character parameter Item
   function From_BMP (Source : BMP_Character_Array) return UXString;
   -- Return an UXString from the array of BMP characters parameter Source

   function Is_Unicode (Source : UXString; Index : Positive) return Boolean;
   -- Return True if the character of Source at Index position is in Unicode set (actually True)
   function Is_Unicode (Source : UXString) return Boolean;
   -- Return True if all the characters of Source are in Unicode set (actually True)
   function Get_Unicode (Source : UXString; Index : Positive) return Unicode_Character;
   -- Return the Unicode character from Source at Index position
   function To_Unicode (Source : UXString) return Unicode_Character_Array;
   -- Return an array of Unicode characters from Source
   function From_Unicode (Item : Unicode_Character) return UXString;
   -- Return an UXString from the Unicode character parameter Item
   function From_Unicode (Source : Unicode_Character_Array) return UXString;
   -- Return an UXString from the array of Unicode characters parameter Source

   function To_UTF_8 (Source : UXString; Output_BOM : Boolean := False) return UTF_8_Character_Array;
   -- Return an array of UTF-8 characters from Source, prepend UTF-8 BOM if Output_BOM is set to True
   function From_UTF_8 (Source : UTF_8_Character_Array) return UXString;
   -- Return an UXString from the array of UTF-8 characters parameter Source,
   -- leading BOM characters are suppressed if any

   function To_UTF_16
     (Source : UXString; Output_Scheme : UTF_16_Encoding_Scheme; Output_BOM : Boolean := False)
      return UTF_16_Character_Array;
   -- Return an array of UTF-16 characters from Source according to the encoding scheme specified by Output_Scheme,
   -- prepend UTF-16 BOM if Output_BOM is set to True
   function From_UTF_16 (Source : UTF_16_Character_Array; Input_Scheme : UTF_16_Encoding_Scheme) return UXString;
   -- Return an UXString from the array of UTF-16 characters parameter Source
   -- according to the encoding scheme specified by Input_Scheme,
   -- leading BOM characters are suppressed if any

   procedure Set (Target : out UXString; Source : Unicode_Character_Array);
   -- Set Target to an UXString from the array of Unicode characters parameter Source

   procedure Append (Source : in out UXString; New_Item : UXString);
   -- Update Source to the concatenation of Source and New_Item
   procedure Append (Source : in out UXString; New_Item : Unicode_Character);
   -- Update Source to the concatenation of Source and New_Item

   procedure Prepend (Source : in out UXString; New_Item : UXString);
   -- Update Source to the concatenation of New_Item and Source
   procedure Prepend (Source : in out UXString; New_Item : Unicode_Character);
   -- Update Source to the concatenation of New_Item and Source

   function "&" (Left : UXString; Right : UXString) return UXString;
   -- Return the concatenation of Left and Right
   function "&" (Left : UXString; Right : Unicode_Character) return UXString;
   -- Return the concatenation of Left and Right
   function "&" (Left : Unicode_Character; Right : UXString) return UXString;
   -- Return the concatenation of Left and Right

   procedure Replace_ASCII (Source : in out UXString; Index : Positive; By : ASCII_Character);
   -- Update Source such as the character at Index position is set to the ASCII character parameter By
   procedure Replace_Latin_1 (Source : in out UXString; Index : Positive; By : Latin_1_Character);
   -- Update Source such as the character at Index position is set to the Latin 1 character parameter By
   procedure Replace_BMP (Source : in out UXString; Index : Positive; By : BMP_Character);
   -- Update Source such as the character at Index position is set to the BMP character parameter By
   procedure Replace_Unicode (Source : in out UXString; Index : Positive; By : Unicode_Character);
   -- Update Source such as the character at Index position is set to the Unicode character parameter By

   function Slice (Source : UXString; Low : Positive; High : Integer) return UXString;
   -- Return the slice at positions Low through High from Source
   procedure Slice (Source : UXString; Target : out UXString; Low : Positive; High : Integer);
   -- Set Target to the slice at positions Low through High from Source

   function "=" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left equals Right
   function "<" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left is less than Right
   function "<=" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left is less or equal than Right
   function ">" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left is greater than Right
   function ">=" (Left : UXString; Right : UXString) return Boolean;
   -- Return True if Left greater or equal than Right

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : UXString; Pattern : UXString; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping := Identity) return Natural;
   -- Return the position of the first character where Pattern matches Source
   -- with respect of Going direction and Mapping
   function Index
     (Source  : UXString; Pattern : UXString; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping_Function) return Natural;
   -- Return the position of the first character where Pattern matches Source
   -- with respect of Going direction and Mapping
   function Index
     (Source : UXString; Set : Wide_Wide_Character_Set; Test : Membership := Inside; Going : Direction := Forward)
      return Natural;
   -- Return the position of the first character inside or outside Set matches Source
   -- with respect of Going direction and Test membership
   function Index
     (Source  : UXString; Pattern : UXString; From : Positive; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping := Identity) return Natural;
   -- Return the position of the first character where Pattern matches Source starting at From position
   -- with respect of Going direction and Mapping
   function Index
     (Source  : UXString; Pattern : UXString; From : Positive; Going : Direction := Forward;
      Mapping : Wide_Wide_Character_Mapping_Function) return Natural;
   -- Return the position of the first character where Pattern matches Source starting at From position
   -- with respect of Going direction and Mapping
   function Index
     (Source : UXString; Set : Wide_Wide_Character_Set; From : Positive; Test : Membership := Inside;
      Going  : Direction := Forward) return Natural;
   -- Return the position of the first character inside or outside Set matches Source starting at From position
   -- with respect of Test membership

   function Index_Non_Blank (Source : UXString; Going : Direction := Forward) return Natural;
   -- Return the position of the first non space character of Source with respect of Going direction
   function Index_Non_Blank (Source : UXString; From : Positive; Going : Direction := Forward) return Natural;
   -- Return the position of the first non space character of Source starting at From position
   -- with respect of Going direction

   function Count
     (Source : UXString; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping := Identity) return Natural;
   -- Return the number of non overlapping occurrences of Pattern matching Source with respect of Mapping
   function Count
     (Source : UXString; Pattern : UXString; Mapping : Wide_Wide_Character_Mapping_Function) return Natural;
   -- Return the number of non overlapping occurrences of Pattern matching Source with respect of Mapping
   function Count (Source : UXString; Set : Wide_Wide_Character_Set) return Natural;
   -- Return the number of occurrences of characters in parameter Set matching Source

   procedure Find_Token
     (Source :     UXString; Set : Wide_Wide_Character_Set; From : Positive; Test : Membership; First : out Positive;
      Last   : out Natural);
   -- Set First to position of the first character inside or outside Set matches Source starting at From position
   -- Set Last to position of the last character inside or outside Set matches Source with respect of Test membership
   procedure Find_Token
     (Source : UXString; Set : Wide_Wide_Character_Set; Test : Membership; First : out Positive; Last : out Natural);
   -- Set First to position of the first character inside or outside Set matches Source
   -- Set Last to position of the last character inside or outside Set matches Source with respect of Test membership

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate (Source : UXString; Mapping : Wide_Wide_Character_Mapping) return UXString;
   -- Return Source updated with respect of Mapping
   procedure Translate (Source : in out UXString; Mapping : Wide_Wide_Character_Mapping);
   -- Update Source with respect of Mapping
   function Translate (Source : UXString; Mapping : Wide_Wide_Character_Mapping_Function) return UXString;
   -- Return Source updated with respect of Mapping
   procedure Translate (Source : in out UXString; Mapping : Wide_Wide_Character_Mapping_Function);
   -- Update Source with respect of Mapping

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice (Source : UXString; Low : Positive; High : Natural; By : UXString) return UXString;
   -- Return Source whom characters with positions from Low to High are replaced with parameter By
   procedure Replace_Slice (Source : in out UXString; Low : Positive; High : Natural; By : UXString);
   -- Update Source whom characters with positions from Low to High are replaced with parameter By
   function Insert (Source : UXString; Before : Positive; New_Item : UXString) return UXString;
   -- Return Source with New_Item inserted at position ahead of parameter Before
   procedure Insert (Source : in out UXString; Before : Natural; New_Item : UXString);
   -- Update Source with New_Item inserted at position ahead of parameter Before
   function Overwrite (Source : UXString; Position : Positive; New_Item : UXString) return UXString;
   -- Return Source whom characters starting at Position are replaced with parameter New_Item
   procedure Overwrite (Source : in out UXString; Position : Positive; New_Item : UXString);
   -- Update Source whom characters starting at Position are replaced with parameter New_Item
   function Delete (Source : UXString; From : Positive; Through : Natural) return UXString;
   -- Return Source whom characters with positions from Low to High are removed
   procedure Delete (Source : in out UXString; From : Positive; Through : Natural);
   -- Update Source whom characters with positions from Low to High are removed
   function Trim (Source : UXString; Side : Trim_End) return UXString;
   -- Return Source with Space characters removed from left, right or both with respect of Side
   procedure Trim (Source : in out UXString; Side : Trim_End);
   -- Update Source with Space characters removed from left, right or both with respect of Side
   function Trim (Source : UXString; Left : Wide_Wide_Character_Set; Right : Wide_Wide_Character_Set) return UXString;
   -- Return Source with leading characters in Left and trailing characters in Right removed
   procedure Trim (Source : in out UXString; Left : Wide_Wide_Character_Set; Right : Wide_Wide_Character_Set);
   -- Update Source with leading characters in Left and trailing characters in Right removed
   function Head (Source : UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space) return UXString;
   -- Return the first characters from Source up to Count concatenated with Pad characters if needed
   procedure Head (Source : in out UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space);
   -- Update Source to the first characters from Source up to Count concatenated with Pad characters if needed
   function Tail (Source : UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space) return UXString;
   -- Return the last characters from Source up to Count concatenated with Pad characters if needed
   procedure Tail (Source : in out UXString; Count : Natural; Pad : Unicode_Character := Wide_Wide_Space);
   -- Update Source to the last characters from Source up to Count concatenated with Pad characters if needed

   function "*" (Left : Natural; Right : UXString) return UXString;
   -- Return Right string duplicated Left times
   function "*" (Left : Natural; Right : Unicode_Character) return UXString;
   -- Return Right character duplicated Left times

   ----------------------------------------------
   -- String Additional Comparison Subprograms --
   ----------------------------------------------

   function Equal_Case_Insensitive (Left, Right : UXString) return Boolean;
   -- Returns True if the strings consist of the same sequence of characters after applying locale-independent
   -- simple case folding, as defined by documents referenced in the note in Clause 1 of ISO/IEC 10646:2011.
   -- Otherwise, returns False.
   function Less_Case_Insensitive (Left, Right : UXString) return Boolean;
   -- Performs a lexicographic comparison of strings Left and Right, converted to lower case.

   -----------------------------------
   -- String Conversion Subprograms --
   -----------------------------------

   function To_Lower (Item : UXString) return UXString;
   -- Returns the corresponding lower-case value for Item if Is_Upper(Item), and returns Item otherwise.
   function To_Upper (Item : UXString) return UXString;
   -- Returns the corresponding upper-case value for Item if Is_Lower(Item) and Item has an upper-case form,
   -- and returns Item otherwise.
   function To_Basic (Item : UXString) return UXString;
   -- Returns the letter corresponding to Item but with no diacritical mark,
   -- if Item is a letter but not a basic letter; returns Item otherwise.

   -----------------------------------
   -- String additional Subprograms --
   -----------------------------------

   function Contains
     (Source : UXString; Pattern : UXString; Sensitivity : Case_Sensitivity := Sensitive) return Boolean;
   -- Return True if Source contains Pattern with respect case of sensitivity

   function Ends_With
     (Source : UXString; Pattern : UXString; Sensitivity : Case_Sensitivity := Sensitive) return Boolean;
   -- Return True if Source ends with pattern with respect case of sensitivity

   function Starts_With
     (Source : UXString; Pattern : UXString; Sensitivity : Case_Sensitivity := Sensitive) return Boolean;
   -- Return True if Source starts with Pattern with respect case of sensitivity

   function Is_Lower (Source : UXString) return Boolean;
   -- Return True if Source is lowercase

   function Is_Upper (Source : UXString) return Boolean;
   -- Return True if Source is uppercase

   function Is_Basic (Source : UXString) return Boolean;
   -- Return True if source is basic (with no diacritical mark)

   function Is_Empty (Source : UXString) return Boolean;
   -- Return True is Source is empty (equal to Null_UXString)

   function Remove
     (Source : UXString; Pattern : Unicode_Character; Sensitivity : Case_Sensitivity := Sensitive) return UXString;
   -- Return Source where every occurrence of Pattern have been removed with respect of case sensitivity
   procedure Remove
     (Source : in out UXString; Pattern : Unicode_Character; Sensitivity : Case_Sensitivity := Sensitive);
   -- Update Source where every occurrence of Pattern have been removed with respect of case sensitivity

   function Remove (Source : UXString; Pattern : UXString; Sensitivity : Case_Sensitivity := Sensitive) return UXString;
   -- Return Source where every occurrence of Pattern have been removed with respect of case sensitivity
   procedure Remove (Source : in out UXString; Pattern : UXString; Sensitivity : Case_Sensitivity := Sensitive);
   -- Update Source where every occurrence of Pattern have been removed with respect of case sensitivity

   function Replace
     (Source : UXString; Before, After : Unicode_Character; Sensitivity : Case_Sensitivity := Sensitive)
      return UXString;
   -- Return a string which has had the before character replaced with the after character
   -- wherever the before character is found with respect of sensitivity
   procedure Replace
     (Source : in out UXString; Before, After : Unicode_Character; Sensitivity : Case_Sensitivity := Sensitive);
   -- Update Source which has had the before character replaced with the after character
   -- wherever the before character is found with respect of sensitivity
   function Replace
     (Source : UXString; Before, After : UXString; Sensitivity : Case_Sensitivity := Sensitive) return UXString;
   -- Return a string which has had the before text replaced with the after text
   -- wherever the before text is found with respect of sensitivity
   procedure Replace (Source : in out UXString; Before, After : UXString; Sensitivity : Case_Sensitivity := Sensitive);
   -- Update Source which has had the before text replaced with the after text
   -- wherever the before text is found with respect of case sensitivity

   ------------------------------
   -- String split Subprograms --
   ------------------------------

   function Split
     (Source           : UXString; Separator : Unicode_Character; Sensitivity : Case_Sensitivity := Sensitive;
      Keep_Empty_Parts : Boolean := True) return UXStrings.Lists.UXString_List;
   -- Return a string list resulting in spliting Source into substrings wherever Separator occurs
   function Split
     (Source           : UXString; Separator : UXString; Sensitivity : Case_Sensitivity := Sensitive;
      Keep_Empty_Parts : Boolean := True) return UXStrings.Lists.UXString_List;
   -- Return a string list resulting in spliting Source into substrings wherever Separator occurs
   function Split
     (Source           : UXString; Separator : Wide_Wide_Character_Set; Test : Membership := Inside;
      Keep_Empty_Parts : Boolean := True) return UXStrings.Lists.UXString_List;
   -- Return a string list resulting in spliting Source into substrings wherever Separator occurs with respect of Test membership

private

   type UXString is new UXString_Vector.Vector with null record;

   procedure Bounded_Move (Source : in out UXString; Target : out UXString; Max : Natural; Last : out Natural);

   procedure UXString_Read (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : out UXString);
   for UXString'Read use UXString_Read;

   procedure UXString_Write (Stream : not null access Ada.Streams.Root_Stream_Type'Class; Item : UXString);
   for UXString'Write use UXString_Write;

   Null_UXString : constant UXString := (UXString_Vector.Empty_Vector with null record);

end UXStrings;
