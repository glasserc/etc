function changelog --description 'Generate a changelog since the last tag'
    set -l last_tag (git describe --tags --abbrev=0)
    git log --reverse --no-merges --format='* %s' {$last_tag}..HEAD
end
