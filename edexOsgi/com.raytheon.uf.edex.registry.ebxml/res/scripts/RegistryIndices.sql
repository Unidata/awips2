CREATE INDEX "internationalstring_localizedString_idx" ON ebxml.internationalstring_localizedstring USING btree (internationalstring_key);
CREATE INDEX registryobject_slot_idx ON ebxml.registryobject_slot USING btree (registryobject_id);
CREATE INDEX value_value_idx ON ebxml.value_value USING btree (value_key);