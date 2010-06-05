Around line 281 in Delphi 2009
----------
  SC_STATUS_TYPE= (
    SC_STATUS_PROCESS_INFO = 0
  );
----------
  SC_STATUS_TYPE= type Byte;
const SC_STATUS_PROCESS_INFO = SC_STATUS_TYPE(0);
type
----------

