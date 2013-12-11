alter table madis drop constraint madis_location_reftime_provider_subprovider_restriction_key;

alter table madis add constraint madis_location_stationid_reftime_provider_subprovider_restr_key UNIQUE (location, stationid, reftime, provider, subprovider, restriction)
