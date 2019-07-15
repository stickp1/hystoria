const contractSource = `
  contract History =
      
      record now =
        { witness  : address,
          moment   : string,
          name     : string,
          upVotes  : int,
          dwVotes  : int }
          
      record state =
        { upPast     : list(now),
          dwPast     : list(now),
          timer      : int,
          nows       : map(int, now),
          nowsLength : int,
          major      : int,
          majorCount : int,
          minor      : int,
          minorCount : int,
          carpeDiem  : int }
      
      function init() =
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
          
      public function getNow(index : int) : now =
        switch(Map.lookup(index, state.nows))
          None    => abort("There was no moment witnessed by this index.")
          Some(x) => x
          
      public function getNowsLength() : int   = state.nowsLength
      
      public function getUpPast() : list(now) = state.upPast
        
      public function getDwPast() : list(now) = state.dwPast
      
      public function getCarpeDiem() : int = state.carpeDiem
      
      public function getMajor() : int = state.major
      
      public function getMinor() : int = state.minor
      
      public stateful function registerNow(moment' : string, name' : string) =
        let now = { witness = Call.caller, moment = moment', name = name', upVotes = 0, dwVotes = 0}
        let index = getNowsLength() + 1
        //put(state{ nows[index] = now, nowsLength = index, minor = index, minorCount = 0 }) here every new registration becomes the least popular -> write late to win 
        if ( state.minor == 0 )
          put(state{ nows[index] = now, nowsLength = index, minor = index, major = index })
        elif( state.minorCount != 0 )
          put(state{ nows[index] = now, nowsLength = index, minor = index }) // here the oldest less reputable wins the title -> vote others to win
        else put(state{ nows[index] = now, nowsLength = index })
         
      public stateful function voteUp(index : int) =              // reinforce positive
        let now = getNow(index)
        Chain.spend(now.witness, Call.value)
        let up_upVotes = now.upVotes + Call.value
        let up_carpeDiem = state.carpeDiem + Call.value
        let up_Nows = state.nows{ [index].upVotes = up_upVotes }
        if ( up_upVotes >= state.majorCount )                      // the reacher wins
          put(state { major = index, majorCount = up_upVotes })
        put(state{ nows = up_Nows, carpeDiem = up_carpeDiem })
        isEvent()
        
      public stateful function voteDown(index : int) =             // kill negative
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
    
      
      public function findSmallest(it : int, minor' : int, nows' : map(int, now)) : int = 
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
      
      public stateful function isEvent() = 
        let upNow = state.nows[state.major]
        let dwNow = state.nows[state.minor]
        if ( state.carpeDiem > 1000000 )
          let up_upPast = state.upPast ++ [upNow]
          let up_dwPast = state.dwPast ++ [dwNow]
          put(state{ minor = 1})
          put(state{ upPast = up_upPast, dwPast = up_dwPast, timer @ t = t + 1, nows = {}, nowsLength = 0, major = 0, majorCount = 0, minor = 0, minorCount = 0, carpeDiem = 0})
        
`;

//Address of the meme voting smart contract on the testnet of the aeternity blockchain
const contractAddress = 'ct_6f9qe8QRdYGJaQHjmvb5CXxm3XZMSp8HDv4bTXuWHexcaa3Ek';
//Create variable for client so it can be used in different functions
var client = null;
//Create a new global array for the memes
var memeArray = [];
//Create a new variable to store the length of the meme globally
var memesLength = 0;

function renderMemes() {
  //Order the memes array so that the meme with the most votes is on top
  memeArray = memeArray.sort(function(a,b){return b.votes-a.votes})
  //Get the template we created in a block scoped variable
  let template = $('#template').html();
  //Use mustache parse function to speeds up on future uses
  Mustache.parse(template);
  //Create variable with result of render func form template and data
  let rendered = Mustache.render(template, {memeArray});
  //Use jquery to add the result of the rendering to our html
  $('#memeBody').html(rendered);
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

  //First make a call to get to know how may memes have been created and need to be displayed
  //Assign the value of meme length to the global variable
  memesLength = await callStatic('getNowsLength', []);

  //Loop over every meme to get all their relevant information
  for (let i = 1; i <= memesLength; i++) {

    //Make the call to the blockchain to get all relevant information on the meme
    const meme = await callStatic('getNow', [i]);

    //Create meme object with  info from the call and push into the array with all memes
    memeArray.push({
      creatorName: meme.name,
      memeUrl: meme.moment,
      index: i,
      votes: meme.upVotes,
    })
  }

  //Display updated memes
  renderMemes();

  //Hide loader animation
  $("#loader").hide();
});

//If someone clicks to vote on a meme, get the input and execute the voteCall
jQuery("#memeBody").on("click", ".voteBtn", async function(event){
  $("#loader").show();
  //Create two new let block scoped variables, value for the vote input and
  //index to get the index of the meme on which the user wants to vote
  let value = $(this).siblings('input').val(),
      index = event.target.id;

  //Promise to execute execute call for the vote meme function with let values
  await contractCall('voteUp', [index], value);

  //Hide the loading animation after async calls return a value
  const foundIndex = memeArray.findIndex(meme => meme.index == event.target.id);
  //console.log(foundIndex);
  memeArray[foundIndex].votes += parseInt(value, 10);

  renderMemes();
  $("#loader").hide();
});

//If someone clicks to register a meme, get the input and execute the registerCall
$('#registerBtn').click(async function(){
  $("#loader").show();
  //Create two new let variables which get the values from the input fields
  const name = ($('#regName').val()),
        url = ($('#regUrl').val());

  //Make the contract call to register the meme with the newly passed values
  await contractCall('registerNow', [url, name], 0);

  //Add the new created memeobject to our memearray
  memeArray.push({
    creatorName: name,
    memeUrl: url,
    index: memeArray.length+1,
    votes: 0,
  })

  renderMemes();
  $("#loader").hide();
});
