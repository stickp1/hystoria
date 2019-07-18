const contractSource = `
  contract History =
      
      record now =
        { witness  : address,          // witness of the moment (address)
          moment   : string,           // moment which might become a part of history
          name     : string,           // witness name
          upVotes  : int,              // amount of up votes
          dwVotes  : int }             // amount of down votes (it's two different games at once!)
          
      record state =
        { upPast     : list(now),      // bright history ( most  upvoted   moments )
          dwPast     : list(now),      // dark   history ( least downvoted moments )
          timer      : int,            // time past since beginning (length of past)
          nows       : map(int, now),  // current "nows" (moments that might become history)
          nowsLength : int,            
          major      : int,            // current most up voted moment (to become bright history)
          majorCount : int,        
          minor      : int,            // current least down voted moment (to become dark history)
          minorCount : int,
          carpeDiem  : int }           // current attention (amount in tokens) spent on this instant across all nows
      
      entrypoint init() =
        { upPast = [],
          dwPast = [],
          timer = 0,
          nows = {},
          nowsLength = 0,
          major = 0,
          majorCount = 0,
          minor = 0,
          minorCount = 0,
          carpeDiem = 0 }
          
      public entrypoint getNow(index : int) : now =
        switch(Map.lookup(index, state.nows))
          None    => abort("There was no moment witnessed by this index.")
          Some(x) => x
          
      public entrypoint getNowsLength() : int   = state.nowsLength
      
      public entrypoint getUpPast() : list(now) = state.upPast
        
      public entrypoint getDwPast() : list(now) = state.dwPast
      
      public entrypoint getPastLength() : int = state.timer
      
      public entrypoint getCarpeDiem() : int = state.carpeDiem
      
      public entrypoint getMajor() : int = state.major
      
      public entrypoint getMinor() : int = state.minor
      
      public stateful entrypoint registerNow(moment' : string, name' : string) =
        let now = { witness = Call.caller, moment = moment', name = name', upVotes = 0, dwVotes = 0}
        let index = getNowsLength() + 1
        if ( state.minor == 0 )                                                                    // if there isn't a minor defined
          put(state{ nows[index] = now, nowsLength = index, minor = index, major = index })
        elif( state.minorCount != 0 )                                                              // if current minor has some down votes
          put(state{ nows[index] = now, nowsLength = index, minor = index }) 
        else put(state{ nows[index] = now, nowsLength = index })                                   // else newly created moment is not the minor
         
      public stateful entrypoint voteUp(index : int) =                                            
        let now = getNow(index)
        Chain.spend(now.witness, Call.value)
        let up_upVotes = now.upVotes + Call.value
        let up_carpeDiem = state.carpeDiem + Call.value
        let up_Nows = state.nows{ [index].upVotes = up_upVotes }
        if ( up_upVotes >= state.majorCount )                      
          put(state { major = index, majorCount = up_upVotes })
        put(state{ nows = up_Nows, carpeDiem = up_carpeDiem })
        isEvent()
        
      public stateful entrypoint voteDown(index : int) =                                           
        let now = getNow(index)
        Chain.spend(now.witness, Call.value)
        let up_dwVotes = now.dwVotes + Call.value
        let up_carpeDiem = state.carpeDiem + Call.value
        let up_Nows = state.nows{ [index].dwVotes = up_dwVotes }
        if ( index == state.minor ) 
          let up_minor = findSmallest(1, index, up_Nows)
          put(state{ minor = up_minor, minorCount = up_Nows[up_minor].dwVotes })
        put(state{ nows = up_Nows, carpeDiem = up_carpeDiem })
        isEvent()
      
      public entrypoint findSmallest(it : int, minor' : int, nows' : map(int, now)) : int = 
        let candidate = nows'[it]
        let minor = nows'[minor']
        let up_minor = minor'
        if (it < state.nowsLength && candidate.dwVotes < minor.dwVotes) // couldn't do it any other way
          findSmallest(it + 1, it, nows')
        elif (it < state.nowsLength)
          findSmallest(it + 1, minor', nows')
        elif (candidate.dwVotes < minor.dwVotes)
          it
        else 
          minor'
      
      public stateful entrypoint isEvent() =                    // has enough attention (amount in tokens) been spent to add to history
        let upNow = state.nows[state.major]
        let dwNow = state.nows[state.minor]
        if ( state.carpeDiem > 1000000 )
          let up_upPast = state.upPast ++ [upNow]
          let up_dwPast = state.dwPast ++ [dwNow]
          put(state{ minor = 1})
          put(state{ upPast = up_upPast, dwPast = up_dwPast, timer @ t = t + 1, nows = {}, nowsLength = 0, major = 0, majorCount = 0, minor = 0, minorCount = 0, carpeDiem = 0})
`;

//Address of the meme voting smart contract on the testnet of the aeternity blockchain
//const contractAddress = 'ct_Zyyr1UZhat5xmx2dCcjASfcCR5WCFYB5TBXVbXd4acE1kUwqv';
const contractAddress = 'ct_VQsaYCZ6jfABevAhGeytCVkm4eeEW1Wc4QPpKv9GHVeXcG3HW';
//Create variable for client so it can be used in different functions
var client = null;
//Create a new global array for the memes
var nowArray = [];
//Create a new variable to store the length of the meme globally
var nowsLength = 0;
// Create a new variable to store the length of the past
var pastLength = 0;
// Create a new variable to store bright history
var upPastArray = [];
// Create a new variable to store dark history
var dwPastArray = [];

var firstRenderUp = false;  // we start with bright history rendered already
var firstRenderDown = true;
var bright = true;

function renderNows() {
  //Order the now array so that the moment with the most votes is on top
  if(bright)
    nowArray = nowArray.sort(function(a,b){return b.upVotes-a.upVotes})
  else
    nowArray = nowArray.sort(function(a,b){return b.dwVotes-a.dwVotes})
  //Get the template we created in a block scoped variable
  let template = $('#template').html();
  //Use mustache parse function to speeds up on future uses
  Mustache.parse(template);
  //Create variable with result of render func form template and data
  let rendered = Mustache.render(template, {nowArray});
  //Use jquery to add the result of the rendering to our html
  $('#nowBody').html(rendered);
}

function renderUpPast(){
  let template = $('#templateUp').html();
  Mustache.parse(template);
  let rendered = Mustache.render(template, {upPastArray});
  $('#upPastBody').html(rendered);
}

function renderDwPast(){
  let template = $('#templateDown').html();
  Mustache.parse(template);
  let rendered = Mustache.render(template, {dwPastArray});
  $('#dwPastBody').html(rendered);
}

//Create a asynchronous read call for our smart contract
async function callStatic(func, args) {
  //Create a new contract instance that we can interact with
  const contract = await client.getContractInstance(contractSource, {contractAddress});
  //Make a call to get data of smart contract func, with specefied arguments
  console.log('args', args);
  console.log('func', func);
  const calledGet = await contract.call(func, args, {callStatic: true}).catch(e => console.error(e));
  console.log('calledGet', calledGet);
  //Make another call to decode the data received in first call
  const decodedGet = await calledGet.decode().catch(e => console.error(e));

  return decodedGet;
}

//Create a asynchronous write call for our smart contract
async function contractCall(func, args, value) {
  const contract = await client.getContractInstance(contractSource, {contractAddress});
  //Make a call to write smart contract func, with aeon value input
  const calledSet = await contract.call(func, args, {amount: value}).catch(e => console.error(e));
  return calledSet;
}

//Execute main function
window.addEventListener('load', async () => {
  //Display the loader animation so the user knows that something is happening
  $("#loader").show();

  //Initialize the Aepp object through aepp-sdk.browser.js, the base app needs to be running.
  client = await Ae.Aepp();

  //First make a call to get to know how may moments have been created and need to be displayed
  //Assign the value of now length to the global variable
  
  const upPast = await callStatic('getUpPast', []);
  
  const dwPast = await callStatic('getDwPast', []);
  
  upPast.forEach(writeUpPast)
  
  dwPast.forEach(writeDwPast)
  
  console.log("upPastArray", upPastArray)
  
  console.log("dwPastArray", dwPastArray)
  
  renderUpPast();
  
  nowsLength = await callStatic('getNowsLength', []);
  
  //Loop over every moment to get all their relevant information
  for (let i = 1; i <= nowsLength; i++) {

    //Make the call to the blockchain to get all relevant information on the moment
    const now = await callStatic('getNow', [i]);

    //Create now object with  info from the call and push into the array with all moments
    nowArray.push({
      witness: now.name,
      moment: now.moment,
      indexUp: i,
      indexDown: -i,
      upVotes: now.upVotes,
      dwVotes: now.dwVotes,
    })
  }

  //Display updated memes
  renderNows();

  //Hide loader animation
  $("#loader").hide();
});

//If someone clicks to vote on a moment, get the input and execute the voteCall
jQuery("#nowBody").on("click", ".voteBtn", async function(event){
  $("#loader").show();
  //Create two new let block scoped variables, value for the vote input and
  //index to get the index of the moment on which the user wants to vote
  let value = $(this).siblings('input').val(),
      index = event.target.id;
  const major = await callStatic('getMajor', []);
  const minor = await callStatic('getMinor', []);
  console.log("major", major);
  console.log("minor", minor);
  if(index > 0) {
    //Promise to execute execute call for the vote now function with let values
    await contractCall('voteUp', [index], value);
    //Hide the loading animation after async calls return a value
    const foundIndex = nowArray.findIndex(now => now.indexUp == event.target.id);
    console.log(foundIndex);
    nowArray[foundIndex].upVotes += parseInt(value, 10);
  } else {
    index = -index;
    //Promise to execute execute call for the vote now function with let values
    await contractCall('voteDown', [index], value);
    //Hide the loading animation after async calls return a value
    const foundIndex = nowArray.findIndex(now => now.indexDown == event.target.id);
    console.log(foundIndex);
    nowArray[foundIndex].dwVotes += parseInt(value, 10);
  }
  
  nowsLength = await callStatic('getNowsLength', []); 
  
  if(nowsLength == 0) {
    pastLength += 1;
    nowArray = [];
    const majorIndex = nowArray.findIndex(now => now.indexUp == major);
    const minorIndex = nowArray.findIndex(now => now.indexDown == minor);
    writeUpPast(nowArray[majorIndex].moment);
    writeDwPast(nowArray[minorIndex].moment);
    if(bright){
      firstRenderDown = true;
      renderUpPast();
    }else{
      firstRenderUp = true;
      renderDwPast();
    }
  }
  renderNows();
  $("#loader").hide();
});

$(document).on("change","input[type=radio]",function(){
    $('#loader').show();
    if (document.getElementById('upPastBtn').checked) {
        document.body.style.backgroundColor = "#F0FFFF";
        document.getElementById("static_text").style.color="#2F4F4F";
        document.getElementById("registerBtn").style.backgroundColor="#2F4F4F";
        document.getElementById("registerBtn").style.color="#F0FFFF";
        $("#dwPastBody").hide();
        if(firstRenderUp){
          renderUpPast();
          firstRenderUp = false;
        } else $("#upPastBody").show();
        bright = true;
    } else {
        document.body.style.backgroundColor = "#2F4F4F";
        document.getElementById("static_text").style.color="#F0FFFF";
        document.getElementById("registerBtn").style.background="#F0FFFF";
        document.getElementById("registerBtn").style.color="#2F4F4F";
         $("#upPastBody").hide();
        if(firstRenderDown){
          renderDwPast();
          firstRenderDown = false;
        }else $("#dwPastBody").show();
        bright = false;
    }
    renderNows()
    $('#loader').hide();
});

function writeUpPast(now){
  upPastArray.push({ moment : now.moment })
}

function writeDwPast(now){
  dwPastArray.push({ moment : now.moment })
}

//If someone clicks to register a moment, get the input and execute the registerCall
$('#registerBtn').click(async function(){
  $("#loader").show();
  //Create two new let variables which get the values from the input fields
  const name = ($('#regName').val()),
        moment = ($('#regMoment').val());

  //Make the contract call to register the moment with the newly passed values
  await contractCall('registerNow', [moment, name], 0);

  //Add the new created nowObject to our nowArray
  nowArray.push({
    witness: name,
    moment: moment,
    indexUp: nowArray.length+1,
    indexDown: -nowArray.length-1,
    upVotes: 0,
    dwVotes: 0,
  })
  nowsLength += 1;

  renderNows();
  $("#loader").hide();
});
