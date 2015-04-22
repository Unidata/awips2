#! /usr/bin/env python
import BaseRequest;
dataRequest = BaseRequest.BaseRequest("nctext");
dataRequest.setCount(1);
dataRequest.addParameter("issueSite", "KWNB");
return dataRequest.execute();