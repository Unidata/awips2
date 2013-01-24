CREATE INDEX "intlstring_localizedStrings_idx" ON intlstring_localizedstrings USING btree (intlstring_key);
CREATE INDEX "versionInfo_idx" ON versioninfo USING btree (versionname,userversionname);
CREATE INDEX registryobject_slot_idx ON registryobject_slot USING btree (registryobject_id);
CREATE INDEX value_value_idx ON value_value USING btree (value_key);