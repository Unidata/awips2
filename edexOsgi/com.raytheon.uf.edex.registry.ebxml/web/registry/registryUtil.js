	
	
	function callRestService(url,arg){
		var url = "http://"+window.location.host+"/"+url
		if(arg != null){
			url+="/"+arg;
		}
		var client = new XMLHttpRequest();
		client.open("GET", url, false);
		client.setRequestHeader("Content-Type", "text/plain");
		client.send();
		return client.responseText
	}

	function callDataAccessServiceWithArg(func,arg){
		return callRestService("dataDelivery/dataAccess/"+func+"/"+arg);
	}

	function callDataAccessService(func){
		return callRestService("dataDelivery/dataAccess/"+func);
	}