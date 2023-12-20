*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
class zlcl_earth DEFINITION.
PUBLIC SECTION.
   METHODS ENTER_orbits EXPORTING value(r_value) TYPE STRING.
   METHODS leave_orbits EXPORTING value(r_value) TYPE STRING.
ENDCLASS.
class zlcl_earth IMPLEMENTATION.
method ENTER_orbits.
      R_VALUE = 'THE SETTELITE IS ENTRING EARTH ORBIT'.
    ENDMETHOD.
    method leave_orbits.
      R_VALUE = 'THE SETTELITE IS LEAVING EARTH ORBIT'.
    ENDMETHOD.
ENDCLASS.
class zlcl_PLANT1 DEFINITION.
PUBLIC SECTION.
   METHODS ENTER_orbits EXPORTING value(r_value) TYPE STRING.
   METHODS leave_orbits EXPORTING value(r_value) TYPE STRING.
ENDCLASS.
class zlcl_PLANT1 IMPLEMENTATION.
method ENTER_orbits.
      R_VALUE = 'THE SETTELITE IS ENTRING PLANT1 ORBIT'.
    ENDMETHOD.
    method leave_orbits.
      R_VALUE = 'THE SETTELITE IS LEAVING PLANT1 ORBIT'.
    ENDMETHOD.
ENDCLASS.

class zlcl_MARC DEFINITION.
PUBLIC SECTION.
   METHODS ENTER_orbits EXPORTING value(r_value) TYPE STRING.
   METHODS LAND EXPORTING value(r_value) TYPE STRING.
ENDCLASS.
class zlcl_MARC IMPLEMENTATION.
method ENTER_orbits.
      R_VALUE = 'THE SETTELITE IS ENTRING MARC ORBIT'.
    ENDMETHOD.
    method LAND.
      R_VALUE = 'THE SETTELITE IS LAND MARC ORBIT'.
    ENDMETHOD.
ENDCLASS.
