@AbapCatalog.sqlViewName: 'ZIAGENCY100'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agency inferface CDS view'
define view ZI_agency_100 as select from /dmo/agency
association [1] to I_Country as _Country
    on $projection.CountryCode = _Country.Country
{
    
    key /dmo/agency.agency_id as AgencyId,
    @Semantics.text: true
    /dmo/agency.name as AgencyName,
    /dmo/agency.street as AgStreet,
    /dmo/agency.postal_code as PostalCode,
    /dmo/agency.city as City,
    /dmo/agency.country_code as CountryCode,
    /dmo/agency.phone_number as PhoneNumber,
    /dmo/agency.email_address as EmailAddress,
    /dmo/agency.web_address as WebAddress,
//    /dmo/agency.attachment as Attachment,
//    /dmo/agency.mime_type as MimeType,
//    /dmo/agency.filename as Filename,
//    /dmo/agency.local_created_by as LocalCreatedBy,
//    /dmo/agency.local_created_at as LocalCreatedAt,
//    /dmo/agency.local_last_changed_by as LocalLastChangedBy,
//    /dmo/agency.local_last_changed_at as LocalLastChangedAt,
//    /dmo/agency.last_changed_at as LastChangedAt,
//    /dmo/agency.dummy_field as DummyField,
//    /dmo/agency./dmo/zzsloganzag,
    _Country // Make association public
}
