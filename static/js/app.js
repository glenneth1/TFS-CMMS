// TF SAFE CMMS - Frontend JavaScript

document.addEventListener('DOMContentLoaded', function() {
  // Team number autocomplete (100-199)
  const teamInput = document.querySelector('input[name="assigned_to"]');
  if (teamInput) {
    // Create datalist for team numbers
    const datalist = document.createElement('datalist');
    datalist.id = 'team-numbers';
    for (let i = 100; i <= 199; i++) {
      const option = document.createElement('option');
      option.value = 'Team ' + i;
      datalist.appendChild(option);
    }
    document.body.appendChild(datalist);
    teamInput.setAttribute('list', 'team-numbers');
    teamInput.placeholder = 'Team 100-199';
  }
  
  // Facility filter based on site selection
  const siteSelect = document.getElementById('site-select');
  const facilitySelect = document.getElementById('facility-select');
  
  if (siteSelect && facilitySelect) {
    siteSelect.addEventListener('change', function() {
      const selectedSite = this.value;
      const options = facilitySelect.querySelectorAll('option');
      
      options.forEach(function(option) {
        if (!option.value) return; // Skip placeholder
        const optionSite = option.getAttribute('data-site');
        option.style.display = (optionSite === selectedSite || !selectedSite) ? '' : 'none';
      });
      
      // Reset facility selection if current is hidden
      if (facilitySelect.selectedOptions[0]?.style.display === 'none') {
        facilitySelect.value = '';
      }
    });
  }
  
  // Form submission handling for API calls
  // Skip forms that should use normal submission (inspection reports, etc.)
  document.querySelectorAll('form[action^="/api/"]').forEach(function(form) {
    const action = form.getAttribute('action');
    
    // Let these forms submit normally (server handles redirect)
    if (action.includes('/inspection-reports/create') ||
        action.includes('/create-reinspection') ||
        action.includes('/generate-mrf') ||
        action.includes('/mrf/') ||
        action.includes('/dar/') ||
        action.includes('/irp/') ||
        action.includes('/rr/')) {
      return; // Don't intercept, let browser handle normally
    }
    
    form.addEventListener('submit', function(e) {
      e.preventDefault();
      
      const formData = new FormData(form);
      
      fetch(action, {
        method: 'POST',
        body: formData,
        redirect: 'follow'
      })
      .then(response => {
        // If we got redirected, go to the new URL
        if (response.redirected) {
          window.location.href = response.url;
          return;
        }
        // If successful response, reload current page (for updates)
        if (response.ok) {
          if (action.includes('/activity')) {
            form.reset();
          }
          window.location.reload();
          return;
        }
        // Try to parse as JSON for error messages
        return response.json();
      })
      .then(data => {
        if (data && data.status === 'created') {
          // Redirect to new work order
          if (action.includes('work-orders/create') && data.id) {
            window.location.href = '/work-orders/' + data.id;
          } else {
            window.location.reload();
          }
        } else if (data && data.error) {
          alert('Error: ' + data.error);
        }
      })
      .catch(error => {
        // If parsing failed, just reload - the action probably succeeded
        console.log('Response was not JSON, reloading page');
        if (action.includes('/activity')) {
          form.reset();
        }
        window.location.reload();
      });
    });
  });
  
  // Auto-submit filter forms on select change
  document.querySelectorAll('.filter-form select').forEach(function(select) {
    if (!select.hasAttribute('onchange')) {
      select.addEventListener('change', function() {
        // Don't auto-submit, let user click filter button
      });
    }
  });
  
  // Confirm before closing work orders
  const statusSelect = document.querySelector('select[name="status"]');
  if (statusSelect) {
    const form = statusSelect.closest('form');
    if (form) {
      form.addEventListener('submit', function(e) {
        const newStatus = statusSelect.value;
        if (newStatus === 'Closed') {
          if (!confirm('Are you sure you want to close this work order? This action should only be done after QAQC verification.')) {
            e.preventDefault();
          }
        }
      });
    }
  }
});
