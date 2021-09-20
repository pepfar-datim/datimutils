//logic for returning to shiny goes here.
const sendToShiny = (id) => {
  const send_dest = id + "code";
  return (code) => {
    console.log("sending message with code to shiny", send_dest, code);
    return Shiny.onInputChange(send_dest, code);
  };
};

$(document).on('shiny:connected', event => {
    console.log("shiny is connected.");

    //watch for message from server saying it's ready.
    Shiny.addCustomMessageHandler("initialize_button",
        params => {
          console.log("params", params);
          params.onCodeReceive = sendToShiny(params.id);
          const loginButton = OAuthr( params );
        }
    );
});