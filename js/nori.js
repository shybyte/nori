var noriVoice;

function startNori() {
  // get an empty <div>
  var div = document.getElementById('app');

  // embed our Elm program in that <div>
  var noriUI = Elm.embed(Elm.Nori, div, {});

  noriUI.ports.speakPort.subscribe(function (singingWord) {
    if (singingWord) {
      console.log('Singing: ', singingWord);
      noriVoice.sing(singingWord.text + " ", singingWord.freq);
    }
  });
}

function playTestSound() {
  var audioCtx = new (window.AudioContext || window.webkitAudioContext)();
  var oscillator = audioCtx.createOscillator();
  var gainNode = audioCtx.createGain();
  oscillator.connect(gainNode);
  gainNode.connect(audioCtx.destination);
  oscillator.type = 'sine'; // sine wave — other values are 'square', 'sawtooth', 'triangle' and 'custom'
  oscillator.frequency.value = 40; // value in hertz
  oscillator.start();
}



MIDI.loadPlugin({
  soundfontUrl: "./bower_components/midi/examples/soundfont/",
  instrument: "acoustic_grand_piano",
  onprogress: function(state, progress) {
    console.log(state, progress);
  },
  onsuccess: function() {
    var delay = 0; // play one note every quarter second
    var note = 50; // the MIDI note
    var velocity = 127; // how hard the note hits
    // play the note
    MIDI.setVolume(0, 127);
    MIDI.noteOn(0, note, velocity, delay);
    MIDI.noteOff(0, note, delay + 0.75);
    MIDI.noteOn(0, note+2, velocity, delay+2);
    MIDI.noteOff(0, note+2, delay+2 + 0.75);
    noriVoice = nori.facade.startNoriVoice();
    startNori();
  }
});