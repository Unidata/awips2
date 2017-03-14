-- gather distinct lid,ts combinations from HourlyPP, HourlyPC and DailyPP tables
-- do not include ts = P*

select lid, ts
into temp temptable
from hourlypp
where substr(ts, 1, 1) != 'P'
group by 1,2

union

select lid, ts
from hourlypc
where substr(ts, 1, 1) != 'P'
group by 1,2

union

select lid, ts
from dailypp
where substr(ts, 1, 1) != 'P'
group by 1,2
;

-- add info from location table for the lid 

select 
   l.lid,
   'PPD'||t.ts||'ZZ',
   to_char(l.lat,'99.99'),
   to_char(l.lon,'999.99'),
   case 
     when l.elev >= 0 then to_char(l.elev,'99999')
     else '  -999'
   end,
   '9',
   case
     when l.name ISNULL then 'XXXXXXXXXX'
     else l.name
   end
from temptable t, location l
where t.lid=l.lid
order by l.lid asc;
