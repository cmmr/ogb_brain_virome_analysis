#!/usr/bin/env python3
# parsing from:
# blastn -query query.fasta -db your_db -outfmt "6 qseqid sseqid pident length mismatch gaps qseq sseq" -out blast_results.txt
import csv
import sys

def parse_blast_ignore_n(blast_file, output_file):
    with open(blast_file, 'r') as infile, open(output_file, 'w', newline='') as outfile:
        # Match the columns requested in -outfmt 6
        fieldnames = ['qseqid', 'sseqid', 'orig_pident', 'length', 'orig_mismatch', 'gaps', 'qseq', 'sseq', 'corrected_mismatch', 'corrected_pident']
        
        reader = csv.reader(infile, delimiter='\t')
        writer = csv.writer(outfile, delimiter='\t')
        
        # Write headers for the new file
        writer.writerow(fieldnames)
        
        for row in reader:
            if not row:
                continue
            
            qseqid, sseqid, orig_pident, length, orig_mismatch, gaps, qseq, sseq = row
            
            corrected_mismatches = 0
            identities = 0
            valid_alignment_length = 0
            
            # Zip aligned sequences together to compare position by position
            for q_char, s_char in zip(qseq, sseq):
                # Ignore gaps in identity calculations
                if q_char == '-' or s_char == '-':
                    continue
                
                # Check for Ns (case-insensitive)
                if q_char.upper() == 'N' or s_char.upper() == 'N':
                    continue  # Do not count as mismatch, do not count as identity
                
                valid_alignment_length += 1
                
                if q_char.upper() == s_char.upper():
                    identities += 1
                else:
                    corrected_mismatches += 1
            
            # Recalculate percent identity based on corrected criteria
            # Include gaps in total length if you want to mirror standard BLAST formatting
            total_gaps = int(gaps)
            denom = valid_alignment_length + total_gaps
            corrected_pident = (identities / denom) * 100 if denom > 0 else 0.0
            
            # Output row with updated data
            new_row = [
                qseqid, sseqid, orig_pident, length, orig_mismatch, gaps, 
                qseq, sseq, corrected_mismatches, round(corrected_pident, 2)
            ]
            writer.writerow(new_row)

# Run the function
parse_blast_ignore_n(sys.argv[1], sys.argv[2])
