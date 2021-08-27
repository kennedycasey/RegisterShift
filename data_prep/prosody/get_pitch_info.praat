wd_audio$ = "/Volumes/DRIVE/CHILDES/"

onset_offset$ = "timestamped_utts.csv"
timestamped_utts = Read Table from comma-separated file: onset_offset$

selectObject: timestamped_utts
n_tokens = Get number of rows

for t from 1 to 200
# extract utterance based on timing in csv file
	selectObject: timestamped_utts
	transcript_id = Get value: t, "transcript_id"
	item$ = Get value: t, "item"
	corpus_name$ = Get value: t, "corpus_name"
	target_child_name$ = Get value: t, "target_child_name"
	audio_file$ = Get value: t, "audio_file"
	start_s = Get value: t, "media_start"
	end_s = Get value: t, "media_end"

# opens the audio file and zooms in to the relevant utterance
	audio_file$ = wd_audio$ + corpus_name$ + "/" + target_child_name$ + "/0" + audio_file$ + ".wav"
	
	if fileReadable (audio_file$)
		sound = Read from file: audio_file$
		selectObject: sound
		View & Edit
		editor: sound
		Select: start_s, end_s
		Zoom: start_s, end_s
		untitled = Extract selected sound (preserve times)
		Close

# gets pitch info
		selectObject: untitled
		To Pitch... 0.0 75 500 
		pitch_mean = Get mean... start_s end_s Hertz
		pitch_min = Get minimum... 0 0 Hertz Parabolic
		pitch_max = Get maximum... 0 0 Hertz Parabolic
		pitch_range = pitch_min - pitch_max

		selectObject: timestamped_utts
	
		Set numeric value: t, "pitch_mean", pitch_mean
		Set numeric value: t, "pitch_min", pitch_min
		Set numeric value: t, "pitch_max", pitch_max
		Set numeric value: t, "pitch_range", pitch_range

		select all
		minus timestamped_utts
		Remove

	else 
		Set string value: t, "pitch_mean", "audio file missing"
		Set string value: t, "pitch_min", "audio file missing"
		Set string value: t, "pitch_max", "audio file missing"
		Set string value: t, "pitch_range", "audio file missing"

	endif

selectObject: timestamped_utts
Save as comma-separated file: "pitch_info.csv"

endfor