Around line 11258 in Delphi 2010
----------
    szInsert: packed array[0..MAX_PATH-1] of WCHAR   // text such as "Documents", inserted as specified by szMessage;
----------
    szInsert: packed array[0..MAX_PATH-1] of WCHAR;  // text such as "Documents", inserted as specified by szMessage;
----------
