# Fairly comprehensive test featuring != and ternary operators

# Setup
let alice_score = 85
let bob_score = 92
let threshold = 90

# 1. Using != with if statements
puts "1. Not-equal comparisons:"
if alice_score != bob_score
    puts "   Alice and Bob have different scores"
end

if alice_score != threshold
    puts "   Alice's score differs from threshold"
end
puts ""

# 2. Ternary with != 
puts "2. Ternary with != operator:"
let alice_status = alice_score >= threshold ? "pass" : "review"
let bob_status = bob_score >= threshold ? "pass" : "review"

puts "   Alice: #{alice_status}"
puts "   Bob: #{bob_status}"
puts ""

# 3. One-liner ternaries with !=
puts "3. One-liner conditionals:"
alice_score != threshold ? puts "   Alice needs review"
bob_score != threshold ? puts "   Bob needs review (won't print)"
puts ""

# 4. Complex expressions
puts "4. Complex expressions:"
if alice_score != bob_score and alice_score < threshold
    puts "   Alice scored less than Bob and below threshold"
end

if bob_score != alice_score and bob_score >= threshold
    puts "   Bob scored differently and passed"
end
puts ""

# 5. Ternary chains with interpolation
puts "5. String interpolation everywhere:"
let msg = alice_score != bob_score ? "Scores differ by #{bob_score - alice_score}" : "Same score"
puts "   #{msg}"

alice_score >= threshold ? puts "   Alice: #{alice_score} (Pass!)" : puts "   Alice: #{alice_score} (Review)"
bob_score >= threshold ? puts "   Bob: #{bob_score} (Pass!)" : puts "   Bob: #{bob_score} (Review)"
puts ""

# 6. Mixed operators
puts "6. Multiple conditions:"
let both_pass = alice_score >= threshold and bob_score >= threshold
let different = alice_score != bob_score

both_pass ? puts "   Both students passed!" : puts "   Review needed"
different ? puts "   Scores are different" : puts "   Scores are the same"
puts ""
