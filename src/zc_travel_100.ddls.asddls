@AbapCatalog.sqlViewName: 'ZCTRAVEL100'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@EndUserText.label: 'consumption view travel'
define root view zc_travel_100 as select from /dmo/travel as Travel
association [1] to ZI_agency_100 as _Agency
    on $projection.AgencyId = _Agency.AgencyId
    association [1] to zi_customer_100 as _Customer
    on $projection.CustomerId = _Customer.CustomerId
    association [1] to I_Currency  as _Currency
    on $projection.CurrencyCode = _Currency.Currency
{
   
    key Travel.travel_id as TravelId,
    @ObjectModel.text.association: '_Agency'
    Travel.agency_id as AgencyId,
    @ObjectModel.text.association: '_Customer'
    Travel.customer_id as CustomerId,
    Travel.begin_date as BeginDate,
    Travel.end_date as EndDate,
    @Semantics.amount.currencyCode: 'CurrencyCode'    
    Travel.booking_fee as BookingFee,
    @Semantics.amount.currencyCode: 'CurrencyCode' 
    Travel.total_price as TotalPrice,
    Travel.currency_code as CurrencyCode,
    Travel.description as Description,
    Travel.status as Status,
    Travel.createdby as Createdby,
    Travel.createdat as Createdat,
    Travel.lastchangedby as Lastchangedby,
    Travel.lastchangedat as Lastchangedat,
    _Agency,
    _Customer,
    _Currency // Make association public
}
