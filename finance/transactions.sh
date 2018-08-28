
function login(){
   $ch = curl_init();
   curl_setopt($ch, CURLOPT_URL, 'https://www.goldman.com/login/logout.cgi'); //login URL
   curl_setopt ($ch, CURLOPT_POST, 1);
   $postData='
   target=
   &gsid=milchr1
   &login.x=18
   &login.y=1
   &gspw=Freber1';
   curl_setopt ($ch, CURLOPT_POSTFIELDS, $postData);
   curl_setopt ($ch, CURLOPT_COOKIEJAR, 'cookie.txt');
   curl_setopt ($ch, CURLOPT_RETURNTRANSFER, 1);
   $store = curl_exec ($ch);
   return $ch;
}

