var REMOTE_URL = 'https://15008d2f.ngrok.io';

// Identify user.
// Identify which variant is being viewer.
// Ask test platform which variant to buy, given the userID and SKU.
// Display price to user.
// Update add-to-cart hidden form to add correct variantID.

var uid = 123;
var sku = "MEH5680S";
var requestURL = REMOTE_URL+'/bucket/?uid='+uid+'&sku='+sku;
var request = new XMLHttpRequest();
request.open('GET', requestURL);
request.responseType = 'json';
request.send();
request.onload = function() {
  console.log(request.response);
}

function setCookie(name,value,days) {
    var expires = "";
    if (days) {
        var date = new Date();
        date.setTime(date.getTime() + (days*24*60*60*1000));
        expires = "; expires=" + date.toUTCString();
    }
    document.cookie = name + "=" + (value || "")  + expires + "; path=/";
}
function getCookie(name) {
    var nameEQ = name + "=";
    var ca = document.cookie.split(';');
    for(var i=0;i < ca.length;i++) {
        var c = ca[i];
        while (c.charAt(0)==' ') c = c.substring(1,c.length);
        if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
    }
    return null;
}
function eraseCookie(name) {
    document.cookie = name+'=; Max-Age=-99999999;';
}
