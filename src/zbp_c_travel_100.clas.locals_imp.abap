CLASS lhc_zc_travel_100 DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zc_travel_100 RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE zc_travel_100.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE zc_travel_100.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE zc_travel_100.

    METHODS read FOR READ
      IMPORTING keys FOR READ zc_travel_100 RESULT result.


    METHODS map_data IMPORTING im_data  type zc_travel_100
                           RETURNING VALUE(r_bapi_data) type /dmo/s_travel_in.
    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zc_travel_100.

ENDCLASS.

CLASS lhc_zc_travel_100 IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

   METHOD map_data.

     r_bapi_data-agency_id = im_data-AgencyId.
     r_bapi_data-begin_date = im_data-BeginDate.
     r_bapi_data-booking_fee = im_data-BookingFee.
     r_bapi_data-currency_code = im_data-CurrencyCode.
     r_bapi_data-currency_code = im_data-CurrencyCode.
     r_bapi_data-customer_id = im_data-CustomerId.
     r_bapi_data-description = im_data-Description.
     r_bapi_data-end_date = im_data-EndDate.
     r_bapi_data-status = im_data-Status.
     r_bapi_data-total_price = im_data-TotalPrice.

  ENDMETHOD.

  METHOD create.
    data: ls_travel_in type /dmo/s_travel_in,
          ls_travel_out type /dmo/travel,
          lt_messages type /dmo/t_message.
      loop at entities ASSIGNING FIELD-SYMBOL(<fs_travel_create>).
          ls_travel_in = map_data( im_data = CORRESPONDING #( <fs_travel_create> ) ).


            call function '/DMO/FLIGHT_TRAVEL_CREATE'
              EXPORTING
                is_travel             = ls_travel_in
*                it_booking            =
*                it_booking_supplement =
*                iv_numbering_mode     =
              IMPORTING
               es_travel             = ls_travel_out
*                et_booking            =
*                et_booking_supplement =
                et_messages           = lt_messages
              .
          if lt_messages is INITIAL.
             INSERT value #( %cid =  <fs_travel_create>-%cid
                             travelid = ls_travel_out-travel_id ) into table mapped-zc_travel_100.
          else.

            loop at lt_messages TRANSPORTING NO FIELDS where msgty = 'E'  or msgty = 'A' .
              insert value #( %cid = <fs_travel_create>-%cid ) into table failed-zc_travel_100.
            endloop.
          ENDIF.

      endloop.

  ENDMETHOD.

  METHOD update.

  Data: ls_travel_in TYPE /dmo/s_travel_in,
        ls_travel_x TYPE /dmo/s_travel_inx,
        lt_messages TYPE /dmo/t_message.
  loop at entities ASSIGNING FIELD-SYMBOL(<updated>).

  ls_travel_in = map_data( im_data = CORRESPONDING #( <updated> ) ).
  ls_travel_x-agency_id = xsdbool( <updated>-%control-AgencyId = cl_abap_behv=>flag_changed ).
  ls_travel_x-begin_date = xsdbool( <updated>-%control-BeginDate = cl_abap_behv=>flag_changed ).
  ls_travel_x-booking_fee = xsdbool( <updated>-%control-BookingFee = cl_abap_behv=>flag_changed ).
  ls_travel_x-currency_code = xsdbool( <updated>-%control-CurrencyCode = cl_abap_behv=>flag_changed ).
  ls_travel_x-customer_id = xsdbool( <updated>-%control-CustomerId = cl_abap_behv=>flag_changed ).
  ls_travel_x-description = xsdbool( <updated>-%control-Description = cl_abap_behv=>flag_changed ).
  ls_travel_x-end_date = xsdbool( <updated>-%control-EndDate = cl_abap_behv=>flag_changed ).
  ls_travel_x-status = xsdbool( <updated>-%control-Status = cl_abap_behv=>flag_changed ).
  ls_travel_x-total_price = xsdbool( <updated>-%control-TotalPrice = cl_abap_behv=>flag_changed ).
  ls_travel_x-travel_id = <updated>-TravelId.
  ls_travel_in-travel_id = <updated>-TravelId.

  CALL FUNCTION '/DMO/FLIGHT_TRAVEL_UPDATE'
    EXPORTING
      is_travel   = ls_travel_in
      is_travelx  = ls_travel_x
*     it_booking  =
*     it_bookingx =
*     it_booking_supplement  =
*     it_booking_supplementx =
    IMPORTING
*     es_travel   =
*     et_booking  =
*     et_booking_supplement  =
      et_messages = lt_messages.
  LOOP AT lt_messages TRANSPORTING NO FIELDS WHERE msgty = 'E'  OR msgty = 'A' .
    INSERT VALUE #( %cid = <updated>-%cid_ref travelid = <updated>-TravelId ) INTO TABLE failed-zc_travel_100.
  ENDLOOP.

  ENDLOOP.



  ENDMETHOD.

  METHOD delete.

  data: lv_travel_id TYPE /dmo/travel_id ,
        lt_messages TYPE /dmo/t_message.

  loop at keys ASSIGNING FIELD-SYMBOL(<deleted>).

  lv_travel_id = <deleted>-TravelId.
  CALL FUNCTION '/DMO/FLIGHT_TRAVEL_DELETE'
    EXPORTING
      iv_travel_id =  lv_travel_id
    IMPORTING
     et_messages  =  lt_messages
    .

          loop at lt_messages TRANSPORTING NO FIELDS where msgty = 'E'  or msgty = 'A' .
              insert value #( %cid = <deleted>-%cid_ref travelid = <deleted>-TravelId ) into table failed-zc_travel_100.
            endloop.

  endloop.

  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.



ENDCLASS.

CLASS lsc_ZC_TRAVEL_100 DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZC_TRAVEL_100 IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.

   call FUNCTION '/DMO/FLIGHT_TRAVEL_SAVE'.
  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
