@AbapCatalog.sqlViewName: 'ZICUSTOMER100'
@AbapCatalog.compiler.compareFilter: true
 
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'customer table'
define view zi_customer_100 as select from /dmo/customer
association [1] to I_Country as _Country
    on $projection.CountryCode = _Country.Country
{
    key /dmo/customer.customer_id as CustomerId,
     @Semantics.text: true
    /dmo/customer.first_name as FirstName,
    /dmo/customer.last_name as LastName,
    /dmo/customer.title as Title,
    /dmo/customer.street as Street,
    /dmo/customer.postal_code as PostalCode,
    /dmo/customer.city as City,       
    /dmo/customer.country_code as CountryCode,
    /dmo/customer.phone_number as PhoneNumber,
    /dmo/customer.email_address as EmailAddress,
//    /dmo/customer.local_created_by as LocalCreatedBy,
//    /dmo/customer.local_created_at as LocalCreatedAt,
//    /dmo/customer.local_last_changed_by as LocalLastChangedBy,
//    /dmo/customer.local_last_changed_at as LocalLastChangedAt,
//    /dmo/customer.last_changed_at as LastChangedAt,
    _Country // Make association public
}
