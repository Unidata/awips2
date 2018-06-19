#!/usr/bin/env python

# update-state-json-vrh.py - Update's VRH's state.json file prior to installing Qpid SSL certificates onto cpsbn1 and cpsbn2
#
# Modification History
#
# Name                Date         Comments
# ---------------------------------------------------------------------------
# Qihan Zhang	      2017-10-11   DR 20377 - Initial creation

import json

with open('/etc/pki/a2pgca/state/state.json', 'r+') as f:
	data = json.load(f)
	for target in data['targets']:
		if target['name'] == 'cp1f' and target['type'] == 'server':
			target['location_specs'] = ['server:$PX_SERVERS:/awips2/qpid']
	f.seek(0)
	json.dump(data, f, indent=4)
	f.truncate()
