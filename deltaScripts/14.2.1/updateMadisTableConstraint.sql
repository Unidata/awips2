alter table if exists madis drop constraint madis_location_reftime_provider_subprovider_restriction_key;
alter table if exists madis add CONSTRAINT madis_latitude_longitude_stationid_reftime_provider_subprovider UNIQUE (latitude, longitude, stationid, reftime, provider, subprovider, restriction)

