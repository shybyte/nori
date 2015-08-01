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
        MIDI.noteOn(midiCommand.channel, midiCommand.note, (midiCommand.channel + 1)*60 , delay);
        MIDI.noteOff(midiCommand.channel, midiCommand.note, delay + 0.75 + midiCommand.channel);
      }
    });
  });
}

MIDI.loadPlugin({
  //soundfontUrl: "./bower_components/midi/examples/soundfont/",
  soundfontUrl: "soundfont/",
  instrument: ["acoustic_grand_piano", "cello"],
  //instrument: "banjo",
  onprogress: function (state, progress) {
    console.log(state, progress);
  },
  onsuccess: function () {
    MIDI.programChange(0, 0);
    MIDI.programChange(1, 42);  //cello
    MIDI.setVolume(0, 60);
    MIDI.setEffects([]);

    var audioContext = MIDI.getContext();
    var tuna = new Tuna(audioContext);

    var delay = new tuna.Delay({
      feedback: 0.45,    //0 to 1+
      delayTime: 350,    //how many milliseconds should the wet signal be delayed?
      wetLevel: 0.55,    //0 to 1+
      dryLevel: 1,       //0 to 1+
      cutoff: 2000,      //cutoff frequency of the built in lowpass-filter. 20 to 22050
      bypass: 0
    });

    noriVoice = nori.facade.startNoriVoice(audioContext, [delay]);
    startNori();
  }
});