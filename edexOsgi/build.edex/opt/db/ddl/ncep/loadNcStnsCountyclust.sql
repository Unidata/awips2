-- load sat.channel tables

COPY stns.countyclustwfo (wfo, clustername, cntyfipscode) FROM stdin;
AKQ	Southampton+Franklin,VA	51175+51620
AKQ	Hampton+Newport_News,VA	51650+51700
AKQ	Prince_George+Hopewell,VA	51149+51670
AKQ	York+Poquoson,VA	51199+51735
AKQ	Henrico+Richmond,VA	51087+51760
AKQ	James_City+Williamsburg,VA	51095+51830
AKQ	Currituck+CurrituckSound,NC	37053+676330
BOX	Norfolk+Plymouth,MA	25021+25023
FWD	Dallas/FortWorth,TXMetro	48113+48439
DVN	ScottIA+RockIsIL	19163+17161
LSX	St.Louis,MOMetro	29189+29510
\.


COPY stns.countycluststate (state, countyname, cntycitifipscode) FROM stdin;
VA	Albemarle	51003+51540
VA	Alleghany	51005+51580
VA	Augusta	51015+51790+51820
VA	Bedford	51019+51515
VA	Fairfax	51059+51600
VA	Frederick	51069+51840
VA	Greensville	51081+51595
VA	Henry	51089+51690
VA	Pittsylvania	51143+51590
VA	PrinceWilliam	51153+51683+51685
VA	Roanoke	51161+51775+51770
VA	Rockingham	51165+51660
VA	Rockbridge	51163+51678+51530
VA	Washington	51191+51520
VA	Wise	51195+51720
\.
