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

  noriUI.ports.midiPort.subscribe(function (midiCommand) {
    if (midiCommand) {
      console.log('Playing: ', midiCommand);
      var delay = 0.15;
      MIDI.noteOn(0, midiCommand.note, 40, delay);
      MIDI.noteOff(0, midiCommand.note, delay + 0.75);
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
  onprogress: function (state, progress) {
    console.log(state, progress);
  },
  onsuccess: function () {
    MIDI.setVolume(0, 127);
    noriVoice = nori.facade.startNoriVoice();
    startNori();
  }
});