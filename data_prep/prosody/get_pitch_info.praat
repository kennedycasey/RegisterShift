wd_timestamped$ = "timestamped/"
wd_audio$ = "/Volumes/DRIVE/CHILDES/"

file_list = Create Strings as file list: "file_list", wd_timestamped$

selectObject: file_list
n_files = Get number of strings

for f from 27 to n_files
	
	selectObject: file_list
	onset_offset$ = Get string: f
	timestamped_utts = Read Table from comma-separated file: wd_timestamped$ + onset_offset$

	selectObject: timestamped_utts
	n_tokens = Get number of rows

	for t from 1 to 5
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
		audio_file$ = wd_audio$ + corpus_name$ + "/" + target_child_name$ + "/" + audio_file$ + ".wav"
	
		if fileReadable (audio_file$)
			sound = Read from file: audio_file$
			selectObject: sound

			end_time = Get end time

			if end_s - start_s < 0.5
				selectObject: timestamped_utts

				Set string value: t, "pitch_mean", "utterance too short to analyze"
				Set string value: t, "pitch_min", "utterance too short to analyze"
				Set string value: t, "pitch_max", "utterance too short to analyze"
				Set string value: t, "pitch_range", "utterance too short to analyze"


			elif end_s > end_time
				selectObject: timestamped_utts

				Set string value: t, "pitch_mean", "relevant audio clip missing"
				Set string value: t, "pitch_min", "relevant audio clip missing"
				Set string value: t, "pitch_max", "relevant audio clip missing"
				Set string value: t, "pitch_range", "relevant audio clip missing"
		
			else 
				View & Edit
				editor: sound
				Select: start_s, end_s
				Zoom: start_s, end_s
				untitled = Extract selected sound (preserve times)
				Close

# gets pitch info
				selectObject: untitled
				To Pitch... 0 75 500 
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
				minus file_list
				minus timestamped_utts
				Remove
		
			endif

		else 
			Set string value: t, "pitch_mean", "audio file missing"
			Set string value: t, "pitch_min", "audio file missing"
			Set string value: t, "pitch_max", "audio file missing"
			Set string value: t, "pitch_range", "audio file missing"

		endif

	selectObject: timestamped_utts
	Save as comma-separated file: "processed/" + item$ + "_pitch_info.csv"

	endfor

endfor