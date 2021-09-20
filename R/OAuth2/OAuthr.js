const createURLString = (
  {
    main_url,
    response_type,
    api_key,
    redirect_uri,
    scope
  }
) => {
  return [
    main_url,
    '?response_type=' + response_type,
    '&client_id=' +  api_key ,
    '&redirect_uri=' + redirect_uri,
    '&scope=' + scope.join("%20")
  ].join('');
};

const addressBar = () => {
  const info = window.location;
  return {
    main_url: info.href,
    has_code: info.search ? true: false,
    code: info.search,
    go_to: (newUrl) => info.replace(newUrl)
  };
};

const parseCode = (url) => url.split("=")[1];
const buttonText = (dom_target, message) => document.getElementById(dom_target).innerHTML = message;
const clearURLOptions = () => window.history.pushState(null, null, window.location.pathname);

const OAuthr = (
  {
    dom_target,
    main_url,
    response_type = "code",
    api_key,
    scope,
    onCodeReceive = (code) => console.log("Code is, " , code)
  }
) => {
  const page = addressBar(),
        apiQuery = createURLString( { main_url, api_key, response_type, redirect_uri:page.main_url, scope } ),
        code = page.has_code? parseCode(page.code): "no code yet";

  if(page.has_code){
    buttonText(dom_target, "Logged In");
    onCodeReceive(code);
    clearURLOptions(); //hide ugly url.
  } else {
    buttonText(dom_target, "Log In");
    document.getElementById(dom_target).onclick = () => page.go_to(apiQuery);
  }

  return {
    sendToShiny: () => console.log("sending this code to shiny noao", code)
  }
}
