# YDK_ALV_FCAT
Quite often concern ABAP programmer - is the building ALV field catalog.
SAP recommends that you create a table for ALV in the data dictionary, and then no problem getting the catalog.
However, I regularly see SAP code and various third-party where a field catalog filled as you like (append-s, all sorts of macros, loop with setting ...), and I often did so too. 
I'm sick and I decided to solve this problem.
Written by a small group of functions to solve this problem (and not only) which I want to share with you:

``` abap
FUNCTION YDK_ALV_FCAT_BUILD .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(ALV_STRUT_KEY) TYPE  YDK_ALV_STRUTURE_KEY
*"     VALUE(STRUCTURES) OPTIONAL
*"  TABLES
*"      IN_FCAT OPTIONAL
*"      ALV_TAB
*"      FCAT
```
**main parameters:**
* alv_strut_key - Required, Structure name for the program YDK_ALV_FCAT, this is not a dictionary structure, this is just a global name for identifying a set of fields. I usually do it the same as the program name (if there are several ALVs in the program I add postfixes)
* alv_tab - Required, ALV table for which the field catalog is formed
* fcat - Required, result field catalog for alv_tab

**additional parameters:**
* structures - Optional, a string with the enumeration through “,” of dictionary structures / tables from which need to take a description for the fields of the table ALV_TAB. The fields are searched by the field name
* in_fcat - Optional, Here you can specify an existing field catalog in which need to add / correct information, data will be combined and transferred to fcat. Used when correct old programs. IN_FCAT is already filled, but you need to add new fields.

in_fcat and fcat - can be catalogs of any type alv: LVC or SLIS, FM itself determines the type of catalog is passed

# Algorithm
FM analyzes the structure of the ALV_TAB table, searches for descriptions and settings for table fields in the tables YDK_ALV_FCAT and YDK_ALV_FCAT_TXT, and then fills in the FCAT
If there is a field missing in these tables: 
* FM trying to find a field description by the data element 
* saves the template for tuning tables YDK_ALV_FCAT and YDK_ALV_FCAT_TXT
* if you have the powers of a developer proposes to perform catalog tuning

# Catalog setting tool
To edit YDK_ALV_FCAT and YDK_ALV_FCAT_TXT, the program YDK_ALV_FCAT (transaction YDKFCAT) is made - the program is simple, it allows you to 
* customize the fields of the catalog 
* delete old fields
* write the catalog to the transport request

![](ydk_alv_fcat_selection_screen.png)
![](ydk_alv_fcat_program_screen.png)
**List of properties of catalog fields available for customization:**
* TECH - Checkbox, Technical field
* CHECKBOX - Checkbox, Output as checkbox
* EDIT - Checkbox, Ready for input
* HOTSPOT - Checkbox, SingleClick-sensitive
* SP_GROUP - Group key
* LOWERCASE - Checkbox, Lowercase letters allowed/not allowed
* CONVEXIT - Conversion Routine
* EDIT_MASK - ALV control: EditMask for output
* OUTPUTLEN - ALV control: Column width in characters
* IN_FCAT - Checkbox, Settings migrated from inbound catalog
* REPTEXT - Heading
* SCRTEXT_L - Long Field Label
* SCRTEXT_M - Medium Field Label
* SCRTEXT_S - Short Field Label
