var noriVoice;

function startNori() {
  function midiToFreq(midiNote) {
    return (440.0 / 32.0) * (Math.pow(2.0, (midiNote - 9.0) / 12.0));
  }


  // get an empty <div>
  var div = document.getElementById('app');

  // embed our Elm program in that <div>
  var noriUI = Elm.embed(Elm.Nori, div, {});

  noriUI.ports.midiPort.subscribe(function (midiCommands) {
    midiCommands.forEach(function (midiCommand) {
      console.log('Playing: ', midiCommand);
      if (midiCommand.channel === 16) {
        if (midiCommand.phonemes) {
          console.log('Singing: ', midiCommand.phonemes);
          noriVoice.sing(midiCommand.phonemes + " ", midiToFreq(midiCommand.note));
        }
      } else {
        var delay = 0.15;
        MIDI.noteOn(0, midiCommand.note, 20, delay);
        MIDI.noteOff(0, midiCommand.note, delay + 0.75);
      }
    });
  });
}

MIDI.loadPlugin({
  soundfontUrl: "./bower_components/midi/examples/soundfont/",
  instrument: "acoustic_grand_piano",
  onprogress: function (state, progress) {
    console.log(state, progress);
  },
  onsuccess: function () {
    MIDI.setVolume(0, 127);

    MIDI.setEffects([]);

    var audioContext = MIDI.getContext();
    var tuna = new Tuna(audioContext);

    var delay = new tuna.Delay({
      feedback: 0.45,    //0 to 1+
      delayTime: 250,    //how many milliseconds should the wet signal be delayed?
      wetLevel: 0.45,    //0 to 1+
      dryLevel: 1,       //0 to 1+
      cutoff: 2000,      //cutoff frequency of the built in lowpass-filter. 20 to 22050
      bypass: 0
    });

    noriVoice = nori.facade.startNoriVoice(audioContext, [delay]);
    startNori();
  }
});