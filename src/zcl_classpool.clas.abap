CLASS zcl_classpool DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  DATA : ITAB TYPE TABLE OF STRING.
  INTERFACES IF_OO_ADT_CLASSRUN.
  Methods reachtomars.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_classpool IMPLEMENTATION.
  METHOD reachtomars.
  DATA : LV_STR TYPE STRING.
    DATA(LO_EARTH) = NEW ZLCL_EARTH( ).
    DATA(LO_PLANT1) = NEW ZLCL_PLANT1( ).
    DATA(LO_MARC) = NEW ZLCL_MARC( ).
   " leave earth orbit
    LO_EARTH->leave_orbits( IMPORTING R_VALUE = LV_STR ).
    APPEND LV_STR TO ITAB.
   ""enter in plant 1
   LO_PLANT1->enter_orbits( IMPORTING R_VALUE = LV_STR  ).
    APPEND LV_STR TO ITAB.
      ""leave plant 1
   LO_PLANT1->leave_orbits( IMPORTING R_VALUE = LV_STR  ).
    APPEND LV_STR TO ITAB.
   ""enter mars orbit
   LO_MARC->enter_orbits( IMPORTING R_VALUE = LV_STR  ).
    APPEND LV_STR TO ITAB.
   "" land on mars.
  LO_MARC->land( IMPORTING R_VALUE = LV_STR   ).
   APPEND LV_STR TO ITAB.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.
  ME->reachtomars(  ).
  LOOP AT ITAB INTO DATA(WA).
   OUT->write(
     EXPORTING
       data   = WA
*       name   =
*     RECEIVING
*       output =
   ).
   ENDLOOP.
  ENDMETHOD.

ENDCLASS.
