$(document).ready(function() {
    setStatusNotConnected();

    $("#sendButton").click(sendMessage);
    $("#connectButton").click(connect);
    $("#disconnectButton").click(disconnect);
    $("#loginButton").click(login);
    $("#getCharacterListButton").click(getCharacterList);
});

function writeStatus(message) {
    var html = document.createElement("div");
    html.setAttribute("class", "message");
    html.innerHTML = message;
    document.getElementById("status").appendChild(html);
}

// Websockets
function connect() {
    ws = new WebSocket("ws://localhost:8081/ws.yaws");

    ws.onopen = function (evt) {
        writeStatus("connected");
        setStatusConnected();
    }

    ws.onclose = function (evt) {
        writeStatus("disconnected");
        setStatusNotConnected();
    }

    ws.onmessage = function (evt) {
        writeStatus("response: " + evt.data);
    }

    ws.onerror = function (evt) {
        writeStatus("error: " + evt.data);
        setStatusNotConnected();
    }
}

function disconnect() {
    ws.close();

    setStatusNotConnected();
}

function sendMessage() {
    var msg = document.getElementById('messageField').value
    ws.send(msg);
}

function login() {
    var user = $("#login").val();
    var password = $("#password").val();
    ws.send('{"type":"LoginRequest","body":{"user":"' + user + '","password":"' + password + '"}}');

    setStatusAuthenticated();
}

function getCharacterList() {
    ws.send('{"type":"GetCharacterList"}');
}

// FSM
var STATUS_NOT_CONNECTED = 0;
var STATUS_CONNECTED_NOT_AUTH = 1;
var STATUS_CONNECTED_AUTH = 2;
var current_status = STATUS_NOT_CONNECTED;

function setStatusNotConnected()
{
    current_status = STATUS_NOT_CONNECTED;
    setUI();
}
function setStatusConnected()
{
    current_status = STATUS_CONNECTED_NOT_AUTH;
    setUI();
}
function setStatusAuthenticated()
{
    current_status = STATUS_CONNECTED_AUTH;
    setUI();
}

function setUI()
{
    switch(current_status)
    {
        case STATUS_CONNECTED_NOT_AUTH:
            $("#connectButton").attr('disabled', 'disabled');
            $("#disconnectButton").removeAttr('disabled');
            $("#loginButton").removeAttr('disabled');
            $("#getCharacterListButton").attr('disabled', 'disabled');
            $("#sendButton").removeAttr('disabled');
            break;
        case STATUS_CONNECTED_AUTH:
            $("#connectButton").attr('disabled', 'disabled');
            $("#disconnectButton").removeAttr('disabled');
            $("#loginButton").attr('disabled', 'disabled');
            $("#getCharacterListButton").removeAttr('disabled');
            $("#sendButton").removeAttr('disabled');
            break;
        case STATUS_NOT_CONNECTED:
            $("#connectButton").removeAttr('disabled');
            $("#disconnectButton").attr('disabled', 'disabled');
            $("#loginButton").attr('disabled', 'disabled');
            $("#getCharacterListButton").attr('disabled', 'disabled');
            $("#sendButton").attr('disabled', 'disabled');
            break;
    }
}