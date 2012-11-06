$(document).ready(function()
{
    setStatusNotConnected();

    // controls
    $("#sendButton").click(sendMessage);
    $("#connectButton").click(connect);
    $("#disconnectButton").click(disconnect);
    $("#loginButton").click(login);

    // player
    $("#roomDirNW").click("nw", go);
    $("#roomDirN").click("n", go);
    $("#roomDirNE").click("ne", go);
    $("#roomDirW").click("w", go);
    $("#roomDirE").click("e", go);
    $("#roomDirSW").click("sw", go);
    $("#roomDirS").click("s", go);
    $("#roomDirSE").click("se", go);
    $("#chatSendButton").click(chatTalk);
});

function writeStatus(message)
{
    $("#console").prepend('<div class="message">' + message + '</div>');
}

function writeTimeline(message)
{
    $("#timeline").prepend('<div class="message">' + message + '</div>');
}

// Websockets
function connect()
{
    ws = new WebSocket("ws://localhost:8081/ws.yaws");

    ws.onopen = function (evt)
    {
        writeStatus("connected");
        writeTimeline("Connected to server");
        setStatusConnected();
    }

    ws.onclose = function (evt)
    {
        writeStatus("disconnected");
        writeTimeline("Disconnected from server");
        setStatusNotConnected();
    }

    ws.onmessage = function (evt)
    {
        // Write to console
        writeStatus("response: " + evt.data);

        // Process message
        processResponse(evt.data);
    }

    ws.onerror = function (evt)
    {
        writeStatus("error: " + evt.data);
        writeTimeline("ERROR");
        setStatusNotConnected();
    }
}

function disconnect()
{
    ws.close();

    setStatusNotConnected();
}

function sendMessage()
{
    var msg = $("#messageField").val();
    ws.send(msg);
}

function login()
{
    var user = $("#login").val();
    var password = $("#password").val();
    ws.send('{"type":"login_request","body":{"user":"' + user + '","password":"' + password + '"}}');
}

function getCharacterList()
{
    ws.send('{"type":"get_character_list_request","body":"none"}');
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
            $("#chatSendButton").attr('disabled', 'disabled');
            break;
        case STATUS_CONNECTED_AUTH:
            $("#connectButton").attr('disabled', 'disabled');
            $("#disconnectButton").removeAttr('disabled');
            $("#loginButton").attr('disabled', 'disabled');
            $("#getCharacterListButton").removeAttr('disabled');
            $("#sendButton").removeAttr('disabled');
            $("#chatSendButton").attr('disabled', 'disabled');
            break;
        case STATUS_NOT_CONNECTED:
            $("#connectButton").removeAttr('disabled');
            $("#disconnectButton").attr('disabled', 'disabled');
            $("#loginButton").attr('disabled', 'disabled');
            $("#getCharacterListButton").attr('disabled', 'disabled');
            $("#sendButton").attr('disabled', 'disabled');
            $("#chatSendButton").attr('disabled', 'disabled');
            disableAllRoomDirections();
            $("#playersInRoom").empty();
            $("#playersUnseen").empty();
            $("#chatRoom").empty();
            $("#roomName").empty();
            break;
    }
}

function processResponse(data)
{
    var obj = $.parseJSON(data);
    //console.log(obj);
    if(obj == null) return;

    switch (obj.type)
    {
        case "login_response":
            loginResponse(obj);
            break;
        case "get_character_list_response":
            characterList(obj.body);
            break;
        case "load_character_response":
            break;
        case "room_info":
            roomInfo(obj.body);
            break;
        case "seen_by_info":
            playerSeen(obj.body);
            break;
        case "unseen_by_info":
            playerUnseen(obj.body);
            break;
        case "room_chat_talk":
            roomChatTalk(obj.body);
            break;
    }

}

function loginResponse(obj)
{
    if(obj.result == "success")
    {
        writeTimeline("Login success");
        setStatusAuthenticated();
        getCharacterList();
    }
    else
    {
        writeTimeline("Login failed");
        $('#loginFailed').modal('show');
    }
}

function disableAllRoomDirections()
{
    $("#roomDirNW").attr('disabled', 'disabled');
    $("#roomDirN").attr('disabled', 'disabled');
    $("#roomDirNE").attr('disabled', 'disabled');
    $("#roomDirW").attr('disabled', 'disabled');
    $("#roomDirE").attr('disabled', 'disabled');
    $("#roomDirSW").attr('disabled', 'disabled');
    $("#roomDirS").attr('disabled', 'disabled');
    $("#roomDirSE").attr('disabled', 'disabled');
}

function go(direction)
{
    ws.send('{"type":"player_go_request","body":{"direction":"' + direction.data + '"}}');
}

function chatTalk()
{
    var message = $("#chatMessage").val();
    ws.send('{"type":"player_talk_request","body":{"message":"' + message + '"}}');
    $("#chatMessage").val('');
}

function roomInfo(data)
{
    var name = data.name;
    var exits = data.exits;

    writeTimeline("You are in room: " + name);

    $("#roomName").html(name);
    disableAllRoomDirections();
    $.each(exits, function(index, value) {
        switch(value)
        {
            case "nw": $("#roomDirNW").removeAttr('disabled'); break;
            case "n": $("#roomDirN").removeAttr('disabled'); break;
            case "ne": $("#roomDirNE").removeAttr('disabled'); break;
            case "w": $("#roomDirW").removeAttr('disabled'); break;
            case "e": $("#roomDirE").removeAttr('disabled'); break;
            case "sw": $("#roomDirSW").removeAttr('disabled'); break;
            case "s": $("#roomDirS").removeAttr('disabled'); break;
            case "se": $("#roomDirSE").removeAttr('disabled'); break;
        }
    });

    $("#playersUnseen").empty();
    $("#chatSendButton").removeAttr('disabled');
}

function characterList(data)
{
    $("#characterList").empty();

    $.each(data, function(index, value) {
        var id = value.public_id;
        var name = value.name;
        $("#characterList").append('<button onclick="loadCharacter(' + id + ')" class="btn btn-success character-list-button">' + name + '</button>');
    });
}
function loadCharacter(id)
{
    $(".character-list-button").attr('disabled', 'disabled');
    ws.send('{"type":"load_character_request","body":{"character_id":"' + id + '"}}');
    writeTimeline("Character loaded");
}

function playerSeen(data)
{
    var name = data.name;
    var id = data.public_id;
    writeTimeline("You can see " + name);
    $("#playersInRoom").append('<dt id="playerSeen' + id + '">' + name + '</dt>');

    $("#playerUnseen" + id).remove();
}
function playerUnseen(data)
{
    var name = data.name;
    var id = data.public_id;
    var direction = data.direction;
    $("#playerSeen" + id).remove();

    $("#playerUnseen" + id).remove();
    if(direction != "none") $("#playersUnseen").append('<dt id="playerUnseen' + id + '">' + name + ' (' + direction + ')</dt>');
}
function roomChatTalk(data)
{
    var name = data.player_name;
    var message = data.message;
    $("#timeline").prepend('<div class="chat-message">[' + name + ']: ' + message + '</div>');
}
