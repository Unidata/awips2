-- gather distinct lid,ts combinations from Location and Ingestfilter tables

select l.lid, i.ts
into temp temptable
from location l, ingestfilter i
where l.lid = i.lid
and l.lat is not null
and l.lon is not null
and i.pe in ('PP', 'PC')
and substr(i.ts, 1, 1) = 'R'
and i.stg2_input = 'T'
group by 1, 2
order by 1, 2;


-- add info from location table for the lid 

select 
   l.lid,
   'PPH'||t.ts||'ZZ',
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
where t.lid=l.lid;
