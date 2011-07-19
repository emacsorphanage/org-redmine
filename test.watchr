def run_test
  system 'make test'
end

watch('org-redmine.el') { |m| run_test }
watch('test/org-redmine-test.el') { |m| run_test }
